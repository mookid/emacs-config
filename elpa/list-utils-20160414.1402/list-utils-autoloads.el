;;; list-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "list-utils" "list-utils.el" (0 0 0 0))
;;; Generated autoloads from list-utils.el

(let ((loads (get 'list-utils 'custom-loads))) (if (member '"list-utils" loads) nil (put 'list-utils 'custom-loads (cons '"list-utils" loads))))

(require 'cl)

(defstruct tconc head tail)

(autoload 'tconc-list "list-utils" "\
Efficiently append LIST to TC.\n\nTC is a data structure created by `make-tconc'.\n\n(fn TC LIST)" nil nil)

(autoload 'tconc "list-utils" "\
Efficiently append ARGS to TC.\n\nTC is a data structure created by `make-tconc'\n\nWithout ARGS, return the list held by TC.\n\n(fn TC &rest ARGS)" nil nil)

(autoload 'list-utils-cons-cell-p "list-utils" "\
Return non-nil if CELL holds a cons cell rather than a proper list.\n\nA proper list is defined as a series of cons cells in which the\ncdr slot of each cons holds a pointer to the next element of the\nlist, and the cdr slot in the final cons holds nil.\n\nA plain cons cell, for the purpose of this function, is a single\ncons in which the cdr holds data rather than a pointer to the\nnext cons cell, eg\n\n    '(1 . 2)\n\nIn addition, a list which is not nil-terminated is not a proper\nlist and will be recognized by this function as a cons cell.\nSuch a list is printed using dot notation for the last two\nelements, eg\n\n    '(1 2 3 4 . 5)\n\nSuch improper lists are produced by `list*'.\n\n(fn CELL)" nil nil)

(autoload 'list-utils-make-proper-copy "list-utils" "\
Copy a cons cell or improper LIST into a proper list.\n\nIf optional TREE is non-nil, traverse LIST, making proper\ncopies of any improper lists contained within.\n\nOptional RECUR-INTERNAL is for internal use only.\n\nImproper lists consist of proper lists consed to a final\nelement, and are produced by `list*'.\n\n(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-proper-inplace "list-utils" "\
Make a cons cell or improper LIST into a proper list.\n\nImproper lists consist of proper lists consed to a final\nelement, and are produced by `list*'.\n\nIf optional TREE is non-nil, traverse LIST, making any\nimproper lists contained within into proper lists.\n\nOptional RECUR-INTERNAL is for internal use only.\n\nModifies LIST and returns the modified value.\n\n(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-improper-copy "list-utils" "\
Copy a proper LIST into an improper list.\n\nImproper lists consist of proper lists consed to a final\nelement, and are produced by `list*'.\n\nIf optional TREE is non-nil, traverse LIST, making proper\ncopies of any improper lists contained within.\n\nOptional RECUR-INTERNAL is for internal use only.\n\n(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-make-improper-inplace "list-utils" "\
Make proper LIST into an improper list.\n\nImproper lists consist of proper lists consed to a final\nelement, and are produced by `list*'.\n\nIf optional TREE is non-nil, traverse LIST, making any\nproper lists contained within into improper lists.\n\nOptional RECUR-INTERNAL is for internal use only.\n\nModifies LIST and returns the modified value.\n\n(fn LIST &optional TREE RECUR-INTERNAL)" nil nil)

(autoload 'list-utils-linear-subseq "list-utils" "\
Return the linear elements from a partially cyclic LIST.\n\nIf there is no cycle in LIST, return LIST.  If all elements of\nLIST are included in a cycle, return nil.\n\nAs an optimization, CYCLE-LENGTH may be specified if the length\nof the cyclic portion is already known.  Otherwise it will be\ncalculated from LIST.\n\n(fn LIST &optional CYCLE-LENGTH)" nil nil)

(autoload 'list-utils-cyclic-subseq "list-utils" "\
Return any cyclic elements from LIST as a circular list.\n\nThe first element of the cyclic structure is not guaranteed to be\nfirst element of the return value unless FROM-START is non-nil.\n\nTo linearize the return value, use `list-utils-make-linear-inplace'.\n\nIf there is no cycle in LIST, return nil.\n\n(fn LIST &optional FROM-START)" nil nil)

(autoload 'list-utils-cyclic-length "list-utils" "\
Return the number of cyclic elements in LIST.\n\nIf some portion of LIST is linear, only the cyclic\nelements will be counted.\n\nIf LIST is completely linear, return 0.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-cyclic-p "list-utils" "\
Return non-nil if LIST contains any cyclic structures.\n\nIf optional PERFECT is set, only return non-nil if LIST is a\nperfect non-branching cycle in which the last element points\nto the first.\n\n(fn LIST &optional PERFECT)" nil nil)

(autoload 'list-utils-linear-p "list-utils" "\
Return non-nil if LIST is linear (no cyclic structure).\n\n(fn LIST)" nil nil)

(defalias 'list-utils-improper-p 'list-utils-cons-cell-p)

(autoload 'list-utils-safe-length "list-utils" "\
Return the number of elements in LIST.\n\nLIST may be linear or cyclic.\n\nIf LIST is not really a list, returns 0.\n\nIf LIST is an improper list, return the number of proper list\nelements, like `safe-length'.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-flat-length "list-utils" "\
Count simple elements from the beginning of LIST.\n\nStop counting when a cons is reached.  nil is not a cons,\nand is considered to be a \"simple\" element.\n\nIf the car of LIST is a cons, return 0.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-make-linear-copy "list-utils" "\
Return a linearized copy of LIST, which may be cyclic.\n\nIf optional TREE is non-nil, traverse LIST, substituting\nlinearized copies of any cyclic lists contained within.\n\n(fn LIST &optional TREE)" nil nil)

(autoload 'list-utils-make-linear-inplace "list-utils" "\
Linearize LIST, which may be cyclic.\n\nModifies LIST and returns the modified value.\n\nIf optional TREE is non-nil, traverse LIST, linearizing any\ncyclic lists contained within.\n\n(fn LIST &optional TREE)" nil nil)

(autoload 'list-utils-safe-equal "list-utils" "\
Compare LIST-1 and LIST-2, which may be cyclic lists.\n\nLIST-1 and LIST-2 may also contain cyclic lists, which are\neach traversed and compared.  This function will not infloop\nwhen cyclic lists are encountered.\n\nNon-nil is returned only if the leaves of LIST-1 and LIST-2 are\n`equal' and the structure is identical.\n\nOptional TEST specifies a test, defaulting to `equal'.\n\nIf LIST-1 and LIST-2 are not actually lists, they are still\ncompared according to TEST.\n\n(fn LIST-1 LIST-2 &optional TEST)" nil nil)

(autoload 'list-utils-depth "list-utils" "\
Find the depth of LIST, which may contain other lists.\n\nIf LIST is not a list or is an empty list, returns a depth\nof 0.\n\nIf LIST is a cons cell or a list which does not contain other\nlists, returns a depth of 1.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-flatten "list-utils" "\
Return a flattened copy of LIST, which may contain other lists.\n\nThis function flattens cons cells as lists, and\nflattens circular list structures.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-insert-before "list-utils" "\
Look in LIST for ELEMENT and insert NEW-ELEMENT before it.\n\nOptional TEST sets the test used for a matching element, and\ndefaults to `equal'.\n\nLIST is modified and the new value is returned.\n\n(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil)

(autoload 'list-utils-insert-after "list-utils" "\
Look in LIST for ELEMENT and insert NEW-ELEMENT after it.\n\nOptional TEST sets the test used for a matching element, and\ndefaults to `equal'.\n\nLIST is modified and the new value is returned.\n\n(fn LIST ELEMENT NEW-ELEMENT &optional TEST)" nil nil)

(autoload 'list-utils-insert-before-pos "list-utils" "\
Look in LIST for position POS, and insert NEW-ELEMENT before.\n\nPOS is zero-indexed.\n\nLIST is modified and the new value is returned.\n\n(fn LIST POS NEW-ELEMENT)" nil nil)

(autoload 'list-utils-insert-after-pos "list-utils" "\
Look in LIST for position POS, and insert NEW-ELEMENT after.\n\nLIST is modified and the new value is returned.\n\n(fn LIST POS NEW-ELEMENT)" nil nil)

(autoload 'list-utils-and "list-utils" "\
Return the elements of LIST1 which are present in LIST2.\n\nThis is similar to `cl-intersection' (or `intersection') from\nthe cl library, except that `list-utils-and' preserves order,\ndoes not uniquify the results, and exhibits more predictable\nperformance for large lists.\n\nOrder will follow LIST1.  Duplicates may be present in the result\nas in LIST1.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nthe list to be hashed (LIST2 unless FLIP is set).\n\nWhen optional FLIP is set, the sense of the comparison is\nreversed.  When FLIP is set, LIST2 will be the guide for the\norder of the result, and will determine whether duplicates may\nbe returned.  Since this function preserves duplicates, setting\nFLIP can change the number of elements in the result.\n\nPerformance: `list-utils-and' and friends use a general-purpose\nhashing approach.  `intersection' and friends use pure iteration.\nIteration can be much faster in a few special cases, especially\nwhen the number of elements is small.  In other scenarios,\niteration can be much slower.  Hashing has no worst-case\nperformance scenario, although it uses much more memory.  For\nheavy-duty list operations, performance may be improved by\n`let'ing `gc-cons-threshold' to a high value around sections that\nmake frequent use of this function.\n\n(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-not "list-utils" "\
Return the elements of LIST1 which are not present in LIST2.\n\nThis is similar to `cl-set-difference' (or `set-difference') from\nthe cl library, except that `list-utils-not' preserves order and\nexhibits more predictable performance for large lists.  Order will\nfollow LIST1.  Duplicates may be present as in LIST1.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nthe list to be hashed (LIST2 unless FLIP is set).\n\nWhen optional FLIP is set, the sense of the comparison is\nreversed, returning elements of LIST2 which are not present in\nLIST1.  When FLIP is set, LIST2 will be the guide for the order\nof the result, and will determine whether duplicates may be\nreturned.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-xor "list-utils" "\
Return elements which are only present in either LIST1 or LIST2.\n\nThis is similar to `cl-set-exclusive-or' (or `set-exclusive-or')\nfrom the cl library, except that `list-utils-xor' preserves order,\nand exhibits more predictable performance for large lists.  Order\nwill follow LIST1, then LIST2.  Duplicates may be present as in\nLIST1 or LIST2.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nthe list to be hashed (LIST2 unless FLIP is set).\n\nWhen optional FLIP is set, the sense of the comparison is\nreversed, causing order and duplicates to follow LIST2, then\nLIST1.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST1 LIST2 &optional TEST HINT FLIP)" nil nil)

(autoload 'list-utils-uniq "list-utils" "\
Return a uniquified copy of LIST, preserving order.\n\nThis is similar to `cl-remove-duplicates' (or `remove-duplicates')\nfrom the cl library, except that `list-utils-uniq' preserves order,\nand exhibits more predictable performance for large lists.  Order\nwill follow LIST.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nLIST.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-dupes "list-utils" "\
Return only duplicated elements from LIST, preserving order.\n\nDuplicated elements may still exist in the result: this function\nremoves singlets.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nLIST.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-singlets "list-utils" "\
Return only singlet elements from LIST, preserving order.\n\nDuplicated elements may not exist in the result.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nLIST.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-partition-dupes "list-utils" "\
Partition LIST into duplicates and singlets, preserving order.\n\nThe return value is an alist with two keys: 'dupes and 'singlets.\nThe two values of the alist are lists which, if combined, comprise\na complete copy of the elements of LIST.\n\nDuplicated elements may still exist in the 'dupes partition.\n\nTEST is an optional comparison function in the form of a\nhash-table-test.  The default is `equal'.  Other valid values\ninclude `eq' (built-in), `eql' (built-in), `list-utils-htt-='\n(numeric), `list-utils-htt-case-fold-equal' (case-insensitive).\nSee `define-hash-table-test' to define your own tests.\n\nHINT is an optional micro-optimization, predicting the size of\nLIST.\n\nPerformance: see notes under `list-utils-and'.\n\n(fn LIST &optional TEST HINT)" nil nil)

(autoload 'list-utils-alist-or-flat-length "list-utils" "\
Count simple or cons-cell elements from the beginning of LIST.\n\nStop counting when a proper list of non-zero length is reached.\n\nIf the car of LIST is a list, return 0.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-alist-flatten "list-utils" "\
Flatten LIST, which may contain other lists.  Do not flatten cons cells.\n\nIt is not guaranteed that the result contains *only* cons cells.\nThe result could contain other data types present in LIST.\n\nThis function simply avoids flattening single conses or improper\nlists where the last two elements would be expressed as a dotted\npair.\n\n(fn LIST)" nil nil)

(autoload 'list-utils-plist-reverse "list-utils" "\
Return reversed copy of property-list PLIST, maintaining pair associations.\n\n(fn PLIST)" nil nil)

(autoload 'list-utils-plist-del "list-utils" "\
Delete from PLIST the property PROP and its associated value.\n\nWhen PROP is not present in PLIST, there is no effect.\n\nThe new plist is returned; use `(setq x (list-utils-plist-del x prop))'\nto be sure to use the new value.\n\nThis functionality overlaps with the undocumented `cl-do-remf'.\n\n(fn PLIST PROP)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "list-utils" '("list-utils-htt-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; list-utils-autoloads.el ends here
