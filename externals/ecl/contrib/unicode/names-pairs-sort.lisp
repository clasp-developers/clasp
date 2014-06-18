(defparameter *destination*
  (merge-pathnames "../../src/c/unicode/"
		   (or *load-truename* *compile-pathname*)))

(let* ((translated-data (copy-tree *compressed-data*))
       (pairs (copy-tree *paired-data*))
       (first-code (loop for (pair-code . pair) in pairs minimize pair-code))
       (last-code (loop for (pair-code . pair) in pairs maximize pair-code)))
  ;;
  ;; We make sure that for each character there is a unique pair which is not
  ;; used anywhere else
  ;;
  (loop with used-code = (make-array (1+ last-code) :initial-element nil)
     for line in translated-data
     for pair-code = (third line)
     do (cond ((/= (length line) 3)
	       (error "Error in compressed data: too long code ~A" line))
	      ((or (aref used-code pair-code)
		   (< pair-code first-code))
	       (let ((new-pair (cons pair-code 0)))
		 (setf pairs (acons (incf last-code) new-pair pairs)
		       (third line) last-code)))
	      (t
	       (setf (aref used-code pair-code) t))))
  ;;
  ;; We now renumber all pairs.
  ;;
  (let ((translation-table (make-array (1+ last-code) :initial-element nil))
	(counter -1))
    (flet ((add-code (code)
	     (or (aref translation-table code)
		 (setf (aref translation-table code) (incf counter))))
	   (translate (old-code)
	     (or (aref translation-table old-code)
		 (error "Unknown code ~A" old-code))))
      ;; First of all we add the words
      (loop for i from 0 below first-code
	 do (add-code i))
      ;; Then we add all pairs that represent characters, so that they
      ;; are consecutive, too.
      (loop for line in translated-data
	 do (setf (third line) (add-code (third line))))
      ;; Finally, we add the remaining pairs
      (loop for record in pairs
	 do (setf (car record) (add-code (car record))))
      ;; ... and we fix the definitions
      (loop for (code . pair) in pairs
	 do (setf (car pair) (translate (car pair))
		  (cdr pair) (translate (cdr pair))))))
  (defparameter *sorted-compressed-data* translated-data)
  (defparameter *sorted-pairs* (sort pairs #'< :key #'car))
  (print 'finished)
  )

(defparameter *grouped-characters*
  (loop with last-ucd-code = nil
     with start-ucd-code = nil
     with start-code = nil
     with output = '()
     with aux = '()
     for n from (third (first *sorted-compressed-data*))
     for line in *sorted-compressed-data*
     for (ucd-code name code) = line
     do (cond ((/= code n)
	       (error "Codes in *sorted-compressed-data* are not consecutive:~%~A"
		      (cons line (subseq aux 0 10))))
	      ((null start-ucd-code)
	       (setf start-ucd-code ucd-code
		     start-code code))
	      ((= last-ucd-code (1- ucd-code))
	       )
	      (t
	       (push (list start-ucd-code last-ucd-code start-code)
		     output)
	       (setf start-ucd-code ucd-code
		     start-code code)))
       (setf last-ucd-code ucd-code aux (cons line aux))
     finally (return (nreverse output))))

(with-open-file (s (merge-pathnames "ucd_names.h" *destination*)
		   :direction :output
		   :if-exists :supersede)
  (format s "/*
 * UNICODE NAMES DATABASE
 */
#ifndef ECL_UCD_NAMES_H
#define ECL_UCD_NAMES_H 1

#define ECL_UCD_FIRST_PAIR ~D
#define ECL_UCD_TOTAL_PAIRS ~D
#define ECL_UCD_TOTAL_GROUPS ~D
#define ECL_UCD_LARGEST_CHAR_NAME ~D
#define ECL_UCD_TOTAL_NAMES ~D

typedef struct {
  unsigned char codes[4];
} ecl_ucd_names_pair_type;

typedef struct {
  int smallest, largest, pair_code;
} ecl_ucd_names_char_group;

typedef struct {
  unsigned char pair[2];
  unsigned char code[3];
} ecl_ucd_code_and_pair;

extern const ecl_ucd_names_pair_type ecl_ucd_names_pair[ECL_UCD_TOTAL_PAIRS];
extern const ecl_ucd_names_char_group ecl_ucd_names_char[ECL_UCD_TOTAL_GROUPS];
extern const char *ecl_ucd_names_word[ECL_UCD_FIRST_PAIR];
extern const ecl_ucd_code_and_pair ecl_ucd_sorted_pairs[ECL_UCD_TOTAL_NAMES];

#endif
"
	  (1+ *last-word-index*)
	  (length *sorted-pairs*)
	  (length *grouped-characters*)
	  (loop for (code name . rest) in *compressed-data*
	       maximize (length name))
	  (length *compressed-data*)
	  ))

(with-open-file (s (merge-pathnames "ucd_names_pair.c" *destination*)
		   :direction :output
		   :if-exists :supersede)
  (format s "/*
 * Pairs of symbols.
 */

#include <ecl/ecl.h>
#include \"ucd_names.h\"

const ecl_ucd_names_pair_type ecl_ucd_names_pair[ECL_UCD_TOTAL_PAIRS] = {
"
	  (length *sorted-pairs*) (length *sorted-pairs*))
  (loop for i from 0
     for (pair-code . (a . b)) in *sorted-pairs*
     do (format s "~A{~D, ~D, ~D, ~D}~%"
		(if (plusp i) "," "")
		(logand a #xff) (ash a -8)
		(logand b #xff) (ash b -8)
		))
  (format s "};~%"))

(with-open-file (s (merge-pathnames "ucd_names_codes.c" *destination*)
		   :direction :output
		   :if-exists :supersede)
  (format s "/*
 * Sorted character names.
 */

#include <ecl/ecl.h>
#include \"ucd_names.h\"

const ecl_ucd_code_and_pair ecl_ucd_sorted_pairs[ECL_UCD_TOTAL_NAMES] = {
")
  (loop with l = (sort (copy-tree *sorted-compressed-data*) #'string<= :key #'second)
     for (ucd-code name code) in l
     for i from 0
     do (format s "~A{{~D, ~D}, {~D, ~D, ~D}}~%"
		(if (plusp i) "," "")
		(logand code #xff) (ash code -8)
		(logand ucd-code #xff) (logand (ash ucd-code -8) #xff)
		(logand (ash ucd-code -16) #xff)))
  (format s "};"))

(with-open-file (s (merge-pathnames "ucd_names_str.c" *destination*)
		   :direction :output
		   :if-exists :supersede)
  (format s "/*
 * Dictionary words.
 */

#include <ecl/ecl.h>
#include \"ucd_names.h\"

const char *ecl_ucd_names_word[ECL_UCD_FIRST_PAIR] = {
")
  (loop for i from 0
     for c across *words-array*
     do (format s "~A~S~%" (if (plusp i) "," "") (or c "")))
  (format s "};~%"))

(with-open-file (s (merge-pathnames "ucd_names_char.c" *destination*)
		   :direction :output
		   :if-exists :supersede)
  (format s "/*
 * Dictionary words.
 */

#include <string.h>
#include <ecl/ecl.h>
#include \"ucd_names.h\"

const ecl_ucd_names_char_group ecl_ucd_names_char[ECL_UCD_TOTAL_GROUPS] = {
"
	  (length *grouped-characters*))
  (loop for i from 0
     for (start end pair-code) in *grouped-characters*
     do (format s "~A{~D,~D,~D}~%" (if (plusp i) "," "")
		start end pair-code))
  (format s "};

static int
search_pair(ecl_character c)
{
  int mid, low = 0, up = ECL_UCD_TOTAL_GROUPS-1;
  do {
    mid = (up + low) / 2;
    if (c < ecl_ucd_names_char[mid].smallest)
      up = mid-1;
    else if (c > ecl_ucd_names_char[mid].largest)
      low = mid+1;
    else
      return (c - ecl_ucd_names_char[mid].smallest) +
             ecl_ucd_names_char[mid].pair_code;
  } while (low <= up && (low >= 0) && (up < ECL_UCD_TOTAL_GROUPS));
  return -1;
}

static void
fill_pair_name(char *buffer, int pair)
{
  if (pair < ECL_UCD_FIRST_PAIR) {
    strncat(buffer, ecl_ucd_names_word[pair], ECL_UCD_LARGEST_CHAR_NAME+1);
/*
    printf(\"text=%s\\n\", ecl_ucd_names_word[pair]);
 */
  } else {
    const ecl_ucd_names_pair_type p = ecl_ucd_names_pair[pair - ECL_UCD_FIRST_PAIR];
/*
    printf(\"ndx=%d\\n\", pair - ECL_UCD_FIRST_PAIR);
    printf(\"c0=%d\\n\", ecl_ucd_names_pair[pair - ECL_UCD_FIRST_PAIR].codes[0]);
    printf(\"c1=%d\\n\", ecl_ucd_names_pair[pair - ECL_UCD_FIRST_PAIR].codes[1]);
    printf(\"c2=%d\\n\", ecl_ucd_names_pair[pair - ECL_UCD_FIRST_PAIR].codes[2]);
    printf(\"c3=%d\\n\", ecl_ucd_names_pair[pair - ECL_UCD_FIRST_PAIR].codes[3]);
 */
    fill_pair_name(buffer, (((unsigned int)p.codes[1]) << 8) | p.codes[0]);
    fill_pair_name(buffer, (((unsigned int)p.codes[3]) << 8) | p.codes[2]);
  }
}

cl_object
_ecl_ucd_code_to_name(ecl_character c)
{
  int pair = search_pair(c);
  if (pair < 0)
    return ECL_NIL;
  else {
    char buffer[ECL_UCD_LARGEST_CHAR_NAME+1];
    buffer[0] = 0;
    fill_pair_name(buffer, pair);
    return make_base_string_copy(buffer);
  }
}

cl_object
_ecl_ucd_name_to_code(cl_object name)
{
  int mid, low = 0, up = ECL_UCD_TOTAL_NAMES-1;
  int l = ecl_length(name);
  if (l <= ECL_UCD_LARGEST_CHAR_NAME) {
    char buffer1[ECL_UCD_LARGEST_CHAR_NAME+1];
    char buffer2[ECL_UCD_LARGEST_CHAR_NAME+1];
    for (mid = 0; mid < l; mid++) {
      ecl_character c = ecl_char_upcase(ecl_char(name, mid));
      buffer1[mid] = c;
      if (c < 32 || c > 127) /* All character names are [-A-Z_0-9]* */
	return ECL_NIL;
    }
    buffer1[mid] = 0;
    do {
      ecl_ucd_code_and_pair p = ecl_ucd_sorted_pairs[mid = (low + up) / 2];
      int flag, pair = ((unsigned int)p.pair[1] << 8) | p.pair[0];
      buffer2[0] = 0;
      fill_pair_name(buffer2, pair);
      flag = strcmp(buffer1, buffer2);
/*
      printf(\"[%d,%d,%d] %s <> (%d)%s -> %d\\n\",
             low, mid, up, buffer1, pair, buffer2, flag);
 */
      if (flag == 0) {
        return ecl_make_fixnum(((unsigned int)p.code[2] << 16) |
                               ((unsigned int)p.code[1] << 8) |
                               p.code[0]);
      } else if (flag < 0) {
        up = mid - 1;
      } else {
        low = mid + 1;
      }
    } while (low <= up);
  }
  return ECL_NIL;
}

"))

;(ext:run-program "/bin/sh" '("-c" "cp *.c *.h ~/devel/ecl/src/c/unicode/"))