;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sharlatan Hellseher <sharlatanus@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; This manifest is a list of packages that user care about (collected via an
;;; online form).  It corresponds to the “Important packages” of GCD 005.

(specifications->manifest
 (list "python-lxml"
       "python-sortedcontainers"
       "python-hypothesis"
       "python-elementpath"
       "python-hatch-fancy-pypi-readme"
       "python-attrs-bootstrap"
       "python-xmlschema"
       "python-pytest"
       "python-cython"
       "python-pytest-asyncio"
       "python-pytest-mock"
       "python-filelock"
       "python-execnet"
       "python-pytest-xdist"
       "python-markupsafe"
       "python-pyyaml"
       "python-ply"))
