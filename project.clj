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

(defproject recruitment_info "0.0.1"
  :description "Recruitment info from BAM files."
  :url "https://github.com/mooreryan/recruitment_info"
  :license {:name "GNU General Public License, GPLv3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [htsjdk/samtools "1.114"]
                 [picard "1.114"]
                 [org.clojure/tools.cli "0.3.1"]]
  :repositories [["local" 
                  {:url ~(str (.toURI (java.io.File. "repo")))}]]
  :profiles {:dev {:dependencies [[speclj "3.0.1"]]}}
  :plugins [[speclj "3.0.1"]]
  :test-paths ["spec"]
  :main recruitment_info.core)
