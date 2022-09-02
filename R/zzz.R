#-------------------------------------------------------------------------------
#
# Package tictoc
#
# Initialization.
#
# Sergei Izrailev, 2011-2012, 2022
#-------------------------------------------------------------------------------
# Copyright 2011-2014 Collective, Inc.
# Portions are Copyright (C) 2017-2022 Jabiru Ventures LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#-------------------------------------------------------------------------------

# Store timing data in our own environment.
tictoc_pkg_env <- new.env(parent = emptyenv())

#-------------------------------------------------------------------------------

.onLoad <- function(libname, pkgname)
{
   tictoc_pkg_env$.ticmsg <- Stack()
   tictoc_pkg_env$.tictoc <- Stack()
   tictoc_pkg_env$.ticlog <- List()
}

#-------------------------------------------------------------------------------
