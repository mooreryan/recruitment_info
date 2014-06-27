;; Copyright 2014 Ryan Moore

;; This file is part of recruitment_info.

;; recruitment_info is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; recruitment_info is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with recruitment_info.  If not, see
;; <http://www.gnu.org/licenses/>.

(ns recruitment_info.plots-spec
  (:require [speclj.core :refer :all]
            [recruitment_info.plots :refer :all]))

(describe "r"
  (with working-script "sum <- 2 + 2;cat(sum)")
  (with broken-script "sum <- 2 + 2;cat(sum))")
  (context "the R script terminates with exit code 0"
    (it "returns the standard out as a string"
      (should= "4"
               (r @working-script))))
  (context "the R script terminates with failures"
    (it "returns the stderr output"
      (should= (str "Error: unexpected ')' in \"cat(sum))\"\n"
                    "Execution halted\n")
               (r @broken-script)))))

(describe "plot-cov"
  (with outdir (str "/Users/ryanmoore/projects/wommack/recruitment_info/"
                    "test_files/test_output"))
  (with reference "Contig45")
  (with id "proper_frags_only")
  (it "saves a coverage plot in the given outdir with the reference name"
    (plot-cov [[1 2 3] [2 3 4] [3 4 5] [3 4 5] [10 11 12 13 14]]
              @reference
              @outdir
              @id)
    (should (.exists 
             (clojure.java.io/file (format "%s/%s_cov_%s.pdf" 
                                           @outdir @reference @id))))))
