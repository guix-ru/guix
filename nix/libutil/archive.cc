#define _XOPEN_SOURCE 600

#include "config.h"

#include <cerrno>
#include <map>
#include <format>

#include <strings.h> // for strcasecmp

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>

#include "archive.hh"
#include "util.hh"


namespace nix {

static string archiveVersion1 = "nix-archive-1";

static string caseHackSuffix = "~nix~case~hack~";


static void dumpContents(const Path & path, size_t size,
    Sink & sink)
{
    writeString("contents", sink);
    writeLongLong(size, sink);

    AutoCloseFD fd = open(path.c_str(), O_RDONLY);
    if (fd == -1) throw SysError(std::format("opening file `{}'", path));

    unsigned char buf[65536];
    size_t left = size;

    while (left > 0) {
        size_t n = left > sizeof(buf) ? sizeof(buf) : left;
        readFull(fd, buf, n);
        left -= n;
        sink(buf, n);
    }

    writePadding(size, sink);
}


static void dump(const Path & path, Sink & sink, PathFilter & filter)
{
    struct stat st;
    if (lstat(path.c_str(), &st))
        throw SysError(std::format("getting attributes of path `{}'", path));

    writeString("(", sink);

    if (S_ISREG(st.st_mode)) {
        writeString("type", sink);
        writeString("regular", sink);
        if (st.st_mode & S_IXUSR) {
            writeString("executable", sink);
            writeString("", sink);
        }
        dumpContents(path, (size_t) st.st_size, sink);
    }

    else if (S_ISDIR(st.st_mode)) {
        writeString("type", sink);
        writeString("directory", sink);

        /* If we're on a case-insensitive system like Mac OS X, undo
           the case hack applied by restorePath(). */
        std::map<string, string> unhacked;
        for (auto & i : readDirectory(path))
	    unhacked[i.name] = i.name;

        for (auto & i : unhacked)
            if (filter(path + "/" + i.first)) {
                writeString("entry", sink);
                writeString("(", sink);
                writeString("name", sink);
                writeString(i.first, sink);
                writeString("node", sink);
                dump(path + "/" + i.second, sink, filter);
                writeString(")", sink);
            }
    }

    else if (S_ISLNK(st.st_mode)) {
        writeString("type", sink);
        writeString("symlink", sink);
        writeString("target", sink);
        writeString(readLink(path), sink);
    }

    else throw Error(std::format("file `{}' has an unsupported type", path));

    writeString(")", sink);
}


void dumpPath(const Path & path, Sink & sink, PathFilter & filter)
{
    writeString(archiveVersion1, sink);
    dump(path, sink, filter);
}


static SerialisationError badArchive(string s)
{
    return SerialisationError("bad archive: " + s);
}


#if 0
static void skipGeneric(Source & source)
{
    if (readString(source) == "(") {
        while (readString(source) != ")")
            skipGeneric(source);
    }
}
#endif


static void parseContents(ParseSink & sink, Source & source, const Path & path)
{
    unsigned long long size = readLongLong(source);

    sink.preallocateContents(size);

    unsigned long long left = size;
    unsigned char buf[65536];

    while (left) {
        checkInterrupt();
        unsigned int n = sizeof(buf);
        if ((unsigned long long) n > left) n = left;
        source(buf, n);
        sink.receiveContents(buf, n);
        left -= n;
    }

    readPadding(size, source);
}


struct CaseInsensitiveCompare
{
    bool operator() (const string & a, const string & b) const
    {
        return strcasecmp(a.c_str(), b.c_str()) < 0;
    }
};

/* Used to place a consistent upper bound on recursion depth */
#define DIRECTORY_NESTING_LIMIT 256

static void parse(ParseSink & sink, Source & source, const Path & path,
                  int nestLimit)
{
    if (nestLimit <= 0) throw Error("nar directory nesting limit reached");
    string s;

    s = readString(source);
    if (s != "(") throw badArchive("expected open tag");

    s = readString(source);
    if (s != "type") throw badArchive("expected type tag");

    s = readString(source);

    if (s == "regular") {
        bool executable = false;
        s = readString(source);
        if (s == "executable") {
            executable = true;
            readString(source);
            s = readString(source);
        }

        if (s != "contents") throw badArchive("expected contents tag");

        sink.createRegularFile(path);
        if (executable) sink.isExecutable();
        parseContents(sink, source, path);
        s = readString(source);
        if (s != ")") throw badArchive("expected close tag");
    }
    else if (s == "symlink") {
        s = readString(source);
        if (s != "target") throw badArchive("expected target tag");
        s = readString(source);
        sink.createSymlink(path, s);
        s = readString(source);
        if (s != ")") throw badArchive("expected close tag");
    }
    else if (s == "directory") {
        string prevName;
        sink.createDirectory(path);
        while (true) {
            checkInterrupt();
            s = readString(source);
            if (s == ")") break;
            if (s != "entry") throw badArchive("expected entry tag");

            s = readString(source);
            if (s != "(") throw badArchive("expected entry open tag");

            s = readString(source);
            if (s != "name") throw badArchive("expected name tag");

            s = readString(source);
            string name = s;
            if (name.empty() || name == "." || name == ".."
                || name.find('/') != string::npos
                || name.find((char) 0) != string::npos)
                throw Error(std::format("NAR contains invalid file name `{}'", name));
            if (!prevName.empty() && name <= prevName)
                throw Error("NAR directory is not sorted");

            s = readString(source);
            if (s != "node") throw badArchive("expected node tag");

            parse(sink, source, path + "/" + name, nestLimit - 1);

            s = readString(source);
            if (s != ")") throw badArchive("expected entry close tag");
            prevName = name;
        }
    }
    else throw badArchive("unknown type");
}

/* Unbounded variant that doesn't recurse, uses path as its stack */
static void parse_unbounded(ParseSink & sink, Source & source, Path & path)
{
    string prevName;
    string s;
start:
    s = readString(source);
    if (s != "(") throw badArchive("expected open tag");

    s = readString(source);
    if (s != "type") throw badArchive("expected type tag");

    s = readString(source);

    if (s == "regular") {
        bool executable = false;
        s = readString(source);
        if (s == "executable") {
            executable = true;
            readString(source);
            s = readString(source);
        }

        if (s != "contents") throw badArchive("expected contents tag");

        sink.createRegularFile(path);
        if (executable) sink.isExecutable();
        parseContents(sink, source, path);
        s = readString(source);
        if (s != ")") throw badArchive("expected close tag");
    }
    else if (s == "symlink") {
        s = readString(source);
        if (s != "target") throw badArchive("expected target tag");
        s = readString(source);
        sink.createSymlink(path, s);
        s = readString(source);
        if (s != ")") throw badArchive("expected close tag");
    }
    else if (s == "directory") {
        sink.createDirectory(path);
        while (true) {
            checkInterrupt();
            s = readString(source);
            if (s == ")") break;
            if (s != "entry") throw badArchive("expected entry tag");

            s = readString(source);
            if (s != "(") throw badArchive("expected entry open tag");

            s = readString(source);
            if (s != "name") throw badArchive("expected name tag");

            s = readString(source);
            {
                string name = s;
                if (name.empty() || name == "." || name == ".."
                    || name.find('/') != string::npos
                    || name.find((char) 0) != string::npos)
                    throw Error(std::format("NAR contains invalid file name `{}'", name));
                if (!prevName.empty() && name <= prevName)
                    throw Error("NAR directory is not sorted");

                s = readString(source);
                if (s != "node") throw badArchive("expected node tag");

                /* parse(sink, source, path + "/" + name); */
                path.append("/" + name);
                prevName = "";
                goto start;
            }

        continue_directory:
            s = readString(source);
            if (s != ")") throw badArchive("expected entry close tag");
        }
    }
    else throw badArchive("unknown type");

    string::size_type slash_pos = path.rfind("/");
    if (slash_pos != string::npos) {
        prevName = string(path, slash_pos+1);
        path.erase(slash_pos);
        goto continue_directory;
    }
}


void parseDump(ParseSink & sink, Source & source)
{
    string version;
    try {
        version = readString(source);
    } catch (SerialisationError & e) {
        /* This generally means the integer at the start couldn't be
           decoded.  Ignore and throw the exception below. */
    }
    if (version != archiveVersion1)
        throw badArchive("input doesn't look like a normalized archive");

    parse(sink, source, "", DIRECTORY_NESTING_LIMIT);
    /*
    string file = "";
    parse_unbounded(sink, source, file);
    */
}


struct RestoreSink : ParseSink
{
    Path dstPath;
    AutoCloseFD fd;

    void createDirectory(const Path & path)
    {
        Path p = dstPath + path;
        if (mkdir(p.c_str(), 0777) == -1)
            throw SysError(std::format("creating directory `{}'", p));
    };

    void createRegularFile(const Path & path)
    {
        Path p = dstPath + path;
        fd.close();
        fd = open(p.c_str(), O_CREAT | O_EXCL | O_WRONLY, 0666);
        if (fd == -1) throw SysError(std::format("creating file `{}'", p));
    }

    void isExecutable()
    {
        struct stat st;
        if (fstat(fd, &st) == -1)
            throw SysError("fstat");
        if (fchmod(fd, st.st_mode | (S_IXUSR | S_IXGRP | S_IXOTH)) == -1)
            throw SysError("fchmod");
    }

    void preallocateContents(unsigned long long len)
    {
#if HAVE_POSIX_FALLOCATE
        if (len) {
            errno = posix_fallocate(fd, 0, len);
            /* Note that EINVAL may indicate that the underlying
               filesystem doesn't support preallocation (e.g. on
               OpenSolaris).  Since preallocation is just an
               optimisation, ignore it. */
            if (errno && errno != EINVAL)
                throw SysError(std::format("preallocating file of {} bytes", len));
        }
#endif
    }

    void receiveContents(unsigned char * data, unsigned int len)
    {
        writeFull(fd, data, len);
    }

    void createSymlink(const Path & path, const string & target)
    {
        Path p = dstPath + path;
        nix::createSymlink(target, p);
    }
};


void restorePath(const Path & path, Source & source)
{
    RestoreSink sink;
    sink.dstPath = path;
    parseDump(sink, source);
}


}
