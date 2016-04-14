;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                   Carnegie Mellon University and                      ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1998-2005                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH, CARNEGIE MELLON UNIVERSITY AND THE      ;;
;;;  CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO     ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF EDINBURGH, CARNEGIE ;;
;;;  MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE FOR ANY SPECIAL,    ;;
;;;  INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER          ;;
;;;  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN  AN ACTION   ;;
;;;  OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF     ;;
;;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Cluster Unit selection support (Black and Taylor Eurospeech '97)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  clunits build support
;;;
;;;  There are five stages to this
;;;     Load in all utterances
;;;     Load in their coefficients
;;;     Collect together the units of the same type
;;;     build distance tables from them
;;;     dump features for them
;;;

(require_module 'clunits)  ;; C++ modules support
(require 'clunits)         ;; run time scheme support

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (do_all)
  (let ()

    (format t "Loading utterances and sorting types\n")
    (set! utterances (acost:db_utts_load clunits_params))
    (set! unittypes (acost:find_same_types utterances clunits_params))
    (acost:name_units unittypes)

    (format t "Dumping features for clustering\n")
    (acost:dump_features unittypes utterances clunits_params)

    (format t "Loading coefficients\n")
    (acost:utts_load_coeffs utterances)
    ;; If you are short of diskspace try this
    (acost:disttabs_and_clusters unittypes clunits_params)

    ;; or if you have lots of diskspace try
;    (format t "Building distance tables\n")
;    (acost:build_disttabs unittypes clunits_params)

;    ;; Build the cluster trees (requires disttabs and features)
;    (format t "Building cluster trees\n")
;    (acost:find_clusters (mapcar car unittypes) clunits_params)

    ;; Tidy up and put things together
    (acost:collect_trees (mapcar car unittypes) clunits_params)
    
    (format t "Saving unit catalogue\n")
    (acost:save_catalogue utterances clunits_params)
    
  )
)

(define (do_init)
    (set! utterances (acost:db_utts_load clunits_params))
    (set! unittypes (acost:find_same_types utterances clunits_params))
    (acost:name_units unittypes)
    t)

(define (acost:disttabs_and_clusters unittypes clunits_params)
  "(acost:disttabs_and_custers unittypes)
Cause it uses so much diskspace, build each table individually
and them the cluster, removing the table before moving on to the
next."
  (mapcar
   (lambda (uu)
     (acost:build_disttabs (list uu) clunits_params)
     (acost:find_clusters (list (car uu)) clunits_params)
     (delete-file 
      (format nil "%s/%s/%s%s" 
	     (get_param 'db_dir clunits_params "./")
	     (get_param 'disttabs_dir clunits_params "disttabs/")
	     (car uu)
	     (get_param 'disttabs_ext clunits_params ".disttab")))
     )
   unittypes)
  t)

(define (acost:db_utts_load params)
  "(acost:db_utts_load params)
Load in all utterances identified in database."
  (let ((files (car (cdr (assoc 'files params)))))
    (set! acost:all_utts
	  (mapcar
	   (lambda (fname)
	     (set! utt_seg (Utterance Text fname))
	     (utt.load utt_seg 
		       (string-append 
			(get_param 'db_dir params "./")
			(get_param 'utts_dir params "festival/utts/")
			fname
			(get_param 'utts_ext params ".utt")))
	     utt_seg)
	   files))))

(define (acost:utts_load_coeffs utterances)
  "(acost:utts_load_coeffs utterances)
Loading the acoustic coefficients of for each utterance."
  (mapcar 
   (lambda (utt) (acost:utt.load_coeffs utt clunits_params))
   utterances)
  t)

(define (acost:find_same_types utterances params)
  "(acost:find_same_types utterances)
Find all the stream items of the same type and collect them into
lists of that type."
  (let ((clunit_name_feat (get_param 'clunit_name_feat params "name"))
        (clunit_relation (get_param 'clunit_relation params "Segment")))
    (set! acost:unittypes nil)
    (mapcar 
     (lambda (u)
       (mapcar 
	(lambda (s) 
	  (let ((cname (item.feat s clunit_name_feat)))
	    (if (not (string-equal "ignore" cname))
		(begin
		  (item.set_feat s "clunit_name" (item.feat s clunit_name_feat))
		  (let ((p (assoc (item.feat s "clunit_name") acost:unittypes)))
		    (if p
			(set-cdr! p (cons s (cdr p)))
			(set! acost:unittypes
			      (cons
			       (list (item.feat s "clunit_name") s) 
			       acost:unittypes))))))))
	(utt.relation.items u clunit_relation)))
     utterances)
    (acost:prune_unittypes acost:unittypes params)))

(define (acost:prune_unittypes unittypes params)
  "(acost:prune_unittypes unittypes)
If unit types are complex (contain an _) then remove all unittypes sets 
with less than unittype_prune_threshold (typically 3)."
  (if (string-matches (car (car unittypes)) ".*_.*")
      (let ((ut nil) (pt (get_param 'unittype_prune_threshold params 0)))
	(while unittypes
	 (if (or (eq? pt 0)
		 (> (length (cdr (car unittypes))) pt))
	     (set! ut (cons (car unittypes) ut)))
	 (set! unittypes (cdr unittypes)))
	(reverse ut))
      unittypes))

(define (acost:name_units unittypes)
  "(acost:name_units unittypes)
Names each unit with a unique id and number the occurrences of each type."
  (let ((idnum 0) (tynum 0))
    (mapcar
     (lambda (s)
       (set! tynum 0)
       (mapcar
	(lambda (si)
	  (item.set_feat si "unitid" idnum)
	  (set! idnum (+ 1 idnum))
	  (item.set_feat si "occurid" tynum)
	  (set! tynum (+ 1 tynum)))
	(cdr s))
       (format t "units \"%s\" %d\n" (car s) tynum))
     unittypes)
    (format t "total units %d\n" idnum)
    idnum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generating feature files

(define (acost:dump_features unittypes utterances params)
  "(acost:dump_features unittypes utterances params)
Do multiple passes over the utterances for each unittype and
dump the desired features.  This would be easier if utterances
weren't require for feature functions."
  (mapcar
   (lambda (utype)
     (acost:dump_features_utype 
      (car utype)
      (cdr utype)
      utterances
      params))
   unittypes)
  t)

(define (acost:dump_features_utype utype uitems utterances params)
  "(acost:dump_features_utype utype utterances params)
Dump features for all items of type utype."
  (let ((fd (fopen 
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'feats_dir params "festival/feats/")
	      utype
	      (get_param 'feats_ext params ".feats"))
	     "w"))
	(feats (car (cdr (assoc 'feats params)))))
    (format t "Dumping features for %s\n" utype)
    (mapcar
     (lambda (s)
       (mapcar 
	(lambda (f)
	  (set! fval (unwind-protect (item.feat s f) "0"))
	  (if (or (string-equal "" fval)
		  (string-equal " " fval))
	      (format fd "%l " fval)
	      (format fd "%s " fval)))
	feats)
       (format fd "\n"))
     uitems)
    (fclose fd)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tree building functions

(defvar wagon-balance-size 0)

(define (acost:find_clusters unittypes clunits_params)
"Use wagon to find the best clusters."
  (mapcar
   (lambda (unittype)
     (build_tree unittype clunits_params))
   unittypes)
  t)

(define (build_tree unittype clunits_params)
"Build tree with Wagon for this unittype."
  (let ((command 
	 (format nil "%s -desc %s -data '%s' -balance %s -distmatrix '%s' -stop %s -output '%s' %s"
		 (get_param 'wagon_progname clunits_params "wagon")
		 (if (probe_file
		      (string-append
		       (get_param 'db_dir clunits_params "./")
		       (get_param 'wagon_field_desc clunits_params "wagon")
		       "." unittype))
		     ;; So there can be unittype specific desc files
		     (string-append
		       (get_param 'db_dir clunits_params "./")
		       (get_param 'wagon_field_desc clunits_params "wagon")
		       "." unittype)
		     (string-append
		       (get_param 'db_dir clunits_params "./")
		       (get_param 'wagon_field_desc clunits_params "wagon")))
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'feats_dir clunits_params "festival/feats/")
		  unittype
		  (get_param 'feats_ext clunits_params ".feats"))
		 (get_param 'wagon_balance_size clunits_params 0)
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'disttabs_dir clunits_params "festival/disttabs/")
		  unittype
		  (get_param 'disttabs_ext clunits_params ".disttab"))
		 (get_param 'wagon_cluster_size clunits_params 10)
		 (string-append 
		  (get_param 'db_dir clunits_params "./")
		  (get_param 'trees_dir clunits_params "festival/trees/")
		  unittype
		  (get_param 'trees_ext clunits_params ".tree"))
		 (get_param 'wagon_other_params clunits_params "")
		 )))
    (format t "%s\n" command)
    (system command)))

(define (acost:collect_trees unittypes params)
"Collect the trees into one file as an assoc list"
  (let ((fd (fopen 
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'trees_dir params "festival/trees/")
	      (get_param 'index_name params "all.")
	      (get_param 'trees_ext params ".tree"))
	      "wb"))
	(tree_pref
	     (string-append 
	      (get_param 'db_dir params "./")
	      (get_param 'trees_dir params "festival/trees/")))
	(cluster_prune_limit (get_param 'cluster_prune_limit params 0))
	(cluster_merge (get_param 'cluster_merge params 0)))
    (format fd ";; Autogenerated list of selection trees\n")
    (mapcar
     (lambda (fp)
       (format fd ";; %l %l\n" (car fp) (car (cdr fp))))
     params)
    (format fd "(set! clunits_selection_trees '(\n")
    (mapcar
     (lambda (unit)
       (set! tree (car (load (string-append tree_pref unit ".tree") t)))
       (if (> cluster_prune_limit 0)
	   (set! tree (cluster_tree_prune tree cluster_prune_limit)))
       (if (> cluster_merge 0)
	   (set! tree (tree_merge_leafs tree cluster_merge)))
       (if (boundp 'temp_tree_convert)
	   (set! tree (temp_tree_convert)))
       (pprintf (list unit tree) fd))
     unittypes)
    (format fd "))\n")
    (fclose fd)))

(define (cluster_tree_prune_in_line prune_limit)
"(cluster_tree_prune_in_line)
Prune number of units in each cluster in each tree *by* prune_limit,
if negative, or *to* prune_limit, if positive."
  (set! sucs_select_trees 
        (mapcar
	 (lambda (t)
	     (cluster_tree_prune t prune_limit))
	 sucs_select_trees)))

(define (tree_merge_leafs tree depth)
 "(tree_merge_leafs tree depth)
Merge the leafs of the tree at goven depth.  This allows the trees
to be pruned then the single leafs joined together into larger
clusters (so the viterbi part has something to do)."
 (let ((num_leafs (tree_num_leafs tree)))
   (cond
    ((< num_leafs 2) tree)  ;; already at the foot
    ((< num_leafs depth)
     (tree_collect_leafs tree))
    (t
     (list
      (car tree)
      (tree_merge_leafs (car (cdr tree)) depth)
      (tree_merge_leafs (car (cdr (cdr tree))) depth))))))

(define (tree_num_leafs tree)
  "(tree_num_leafs tree)
Number of leafs of given tree."
  (cond
   ((cdr tree)
    (+
     (tree_num_leafs (car (cdr tree)))
     (tree_num_leafs (car (cdr (cdr tree))))))
   (t
    1)))

(define (tree_collect_leafs tree)
  "(tree_collect_leafs tree)
Combine all units in the leafs."
  (cond
   ((cdr tree)
    (let ((a (tree_collect_leafs (car (cdr tree))))
	  (b (tree_collect_leafs (car (cdr (cdr tree))))))
      (list 
       (list
	(append
	 (caar a)
	 (caar b))
	10.0))))
   (t
    tree)))

(define (cluster_tree_prune tree prune_limit)
"(cluster_tree_prune TREE PRUNE_LIMIT)
Reduce the number of elements in the (CART) tree leaves to PRUNE_LIMIT
removing the ones further from the cluster centre.  Maybe later this should
have guards on minimum number of units that must remain in the tree and
a per unit type limit."
  (cond
   ((cdr tree)  ;; a question
    (list
     (car tree)
     (cluster_tree_prune (car (cdr tree)) prune_limit)
     (cluster_tree_prune (car (cdr (cdr tree))) prune_limit)))
   (t           ;; tree leave
    (list 
     (list
      (remove_n_worst 
       (car (car tree))
       (if (< prune_limit 0)
	   (* -1 prune_limit)
	   (- (length (car (car tree))) prune_limit)))
      (car (cdr (car tree))))))))

(define (remove_n_worst lll togo)
"(remove_n_worst lll togo)
Remove togo worst items from lll."
  (cond
   ((< togo 0)
    lll)
   ((equal? 0 togo)
    lll)
   (t
    (remove_n_worst
     (remove (worst_unit (cdr lll) (car lll)) lll)
     (- togo 1)))))

(define (worst_unit lll worst_so_far)
"(worst_unit lll worst_so_far)
Returns unit with worst score in list."
  (cond
   ((null lll)
    worst_so_far)
   ((< (car (cdr worst_so_far)) (car (cdr (car lll))))
    (worst_unit (cdr lll) (car lll)))
   (t
    (worst_unit (cdr lll) worst_so_far))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save the unit catalogue for use in the run-time index

(define (acost:save_catalogue utterances clunits_params)
  "(acost:save_catalogue utterances clunits_params)
Save the catalogue with named units with times."
 (let ((fd (fopen 
	    (string-append 
	     (get_param 'db_dir clunits_params "./")
	     (get_param 'catalogue_dir clunits_params "trees/")
	     (get_param 'index_name clunits_params "catalogue.")
	     ".catalogue")
	      "wb"))
       (num_units 0)
       )
   (format fd "EST_File index\n")
   (format fd "DataType ascii\n")
   (format fd "NumEntries %d\n"
	   (apply 
	    + (mapcar (lambda (u) 
			(length (utt.relation.items u 'Segment))) utterances)))
   (format fd "IndexName %s\n" (get_param 'index_name clunits_params "cluser"))
   (format fd "EST_Header_End\n")
   (mapcar
    (lambda (u)
      (mapcar
       (lambda (s)
	 (format fd "%s_%s %s %f %f %f\n"
		 (item.feat s "clunit_name")
		 (item.feat s 'occurid)
		 (utt.feat u 'fileid)
		 (item.feat s 'segment_start)
		 (item.feat s 'segment_mid)
		 (item.feat s 'segment_end)))
       (utt.relation.items u 'Segment)))
    utterances)
   (fclose fd)))

(provide 'clunits_build.scm)
