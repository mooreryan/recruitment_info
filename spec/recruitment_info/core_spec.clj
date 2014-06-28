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

;; (ns recruitment_info.core-spec
;;   (:require [speclj.core :refer :all]
;;             [recruitment_info.core :refer :all]))

;; (def base "/Users/ryanmoore/projects/wommack/recruitment_info")
;; (def sorted-bam
;;   (str base "/test_files/unpaired.sorted.bam"))
;; (def bam-index
;;   (str base "/test_files/unpaired.sorted.bam.bai"))



(describe "-main"
  (context "with proper arguments"
    (it "gets alignment info from a sorted bam and index"
      (should-not (-main "-b" sorted-bam "-i" bam-index)))))
