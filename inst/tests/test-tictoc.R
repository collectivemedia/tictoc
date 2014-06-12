#-------------------------------------------------------------------------------
#
# Package tictoc 
#
# tests for tic/toc functionality
# 
# Sergei Izrailev, 2014
#-------------------------------------------------------------------------------
# Copyright 2011-2014 Collective, Inc.
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

context("Test tic/toc and related methods")

#-------------------------------------------------------------------------------

test_that("tic measures elapsed time in seconds", 
{
   x <- proc.time()["elapsed"]; y <- tic()
   
   expect_equal(as.logical(abs(x - y) < 0.01), TRUE)    
   
   tic(); Sys.sleep(2); tm <- toc(quiet = TRUE)
   expect_equal(as.logical(abs(tm$toc - tm$tic - 2) < 0.1), TRUE)    
})   

#-------------------------------------------------------------------------------

test_that("nested tic/tocs work",
{
   quiet <- TRUE
   tic("outer")
      Sys.sleep(1) 
      tic("middle") 
         Sys.sleep(2) 
         tic("inner")
            Sys.sleep(3) 
         tm3 <- toc(quiet = quiet)
      tm2 <- toc(quiet = quiet)
   tm1 <- toc(quiet = quiet)
   
   expect_equal(as.logical(abs(tm3$toc - tm3$tic - 3) < 0.1), TRUE)    
   expect_equal(as.logical(abs(tm2$toc - tm2$tic - 5) < 0.1), TRUE)    
   expect_equal(as.logical(abs(tm1$toc - tm1$tic - 6) < 0.1), TRUE)
   expect_equal(tm1$msg, "outer")
   expect_equal(tm2$msg, "middle")
   expect_equal(tm3$msg, "inner")
})

#-------------------------------------------------------------------------------

test_that("tic.clear works", 
{
   tic()
   Sys.sleep(1) 
   tic() 
   Sys.sleep(2) 
   tic()
   Sys.sleep(3) 
   tm3 <- toc(quiet = TRUE)
   
   # we still have two tic() calls to unwind
   tic.clear()
   tm <- toc(quiet = TRUE)
   expect_equal(is.null(tm), TRUE)
})

#-------------------------------------------------------------------------------

test_that("tic.log works", 
{
   quiet <- TRUE
   tic.clearlog()
   tic("outer")
      Sys.sleep(1) 
      tic("middle") 
         Sys.sleep(2) 
         tic("inner")
            Sys.sleep(3) 
         tm3 <- toc(log = TRUE, quiet = quiet)
      tm2 <- toc(log = TRUE, quiet = quiet)
   tm1 <- toc(log = TRUE, quiet = quiet)
   
   log.txt <- tic.log(format = TRUE)
   log.lst <- tic.log(format = FALSE)
   tic.clearlog()
   
   timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
   timings.true <- c(3, 5, 6)
   expect_equal(all(abs(timings - timings.true) < 0.1), TRUE)
})

#-------------------------------------------------------------------------------

test_that("tic.log works in a loop", 
{
   quiet <- TRUE
   tic.clearlog()
   for (x in 1:10)
   {
      tic(x)
      Sys.sleep(1)
      toc(log = TRUE, quiet = quiet)
   }
   log.lst <- tic.log(format = FALSE)
   tic.clearlog()
   
   timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
   expect_equal((abs(mean(timings) - 1) < 0.01), TRUE)
})

#-------------------------------------------------------------------------------

my.msg.out <- function(tic, toc, msg, info)
{
   if (is.null(msg) || is.na(msg) || length(msg) == 0) outmsg <- paste(round(toc - tic, 3), " seconds elapsed", sep="")
   else outmsg <- paste(info, ": ", msg, ": ", round(toc - tic, 3), " seconds elapsed", sep="")      
}

test_that("func.toc custom message formatting works",
{
   quiet <- TRUE
   tic.clearlog()
   tic("outer")
      Sys.sleep(1) 
      tic("middle") 
         Sys.sleep(2) 
         tic("inner")
            Sys.sleep(3) 
         tm3 <- toc(log = TRUE, quiet = quiet, func.toc = my.msg.out, info = "INFO")
      tm2 <- toc(log = TRUE, quiet = quiet, func.toc = my.msg.out, info = "INFO")
   tm1 <- toc(log = TRUE, quiet = quiet, func.toc = my.msg.out, info = "INFO")
   
   log.txt <- tic.log(format = TRUE)
   tic.clearlog()
   
})

#-------------------------------------------------------------------------------

