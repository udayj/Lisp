(defmacro cond-if (predicate consequent &optional alternative)
  `(cond (,predicate ,consequent)
	 (t ,alternative)))