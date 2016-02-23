(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx )
   ))

(defn spiff [v]
  (let [
    first (get v 0)
    third (get v 2)]
    (+ first third)
    ))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [
    [f s t] v]
    (+ f t)
    ))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [
    [[xbottom ybottom] [xtop ytop]] rectangle
    ]
    (- xtop xbottom)
    ))

(defn height [rectangle]
  (let [
    [[xbottom ybottom] [xtop ytop]] rectangle
    ]
    (- ytop ybottom)
    ))

(defn square? [rectangle]
  (let [
    [[xbottom ybottom] [xtop ytop]] rectangle
    ]
    (== (- ytop ybottom) (- xtop xbottom))
    ))

(defn area [rectangle]
  (let [
    [[xbottom ybottom] [xtop ytop]] rectangle
    ]
    (* (- ytop ybottom) (- xtop xbottom))
    ))

(defn contains-point? [rectangle point]
  (let [
    [[xbottom ybottom] [xtop ytop]] rectangle
      [xpoint ypoint] point
      ]
          (and (<= xbottom xpoint xtop) (<= ybottom ypoint ytop))
    ))

(defn contains-rectangle? [outer inner]
  (let [
        [[xbottom1 ybottom1] [xtop1 ytop1]] outer
        [[xbottom2 ybottom2] [xtop2 ytop2]] inner
      ]
              (and (>= xbottom2 xbottom1) (>= ybottom2 ybottom1)
                (>= xtop1 xtop2) (>= ytop1 ytop2))
          ))

(defn title-length [book]
    (count (:title book)))

(defn author-count [book]
    (count (:authors book)))

(defn multiple-authors? [book]
      (>= (count (:authors book)) 2))

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author))
    )

(defn alive? [author]
  (not (contains? author :death-year )))

(defn element-lengths [collection]
  (map (fn [x] (count x)) collection ))

(defn second-elements [collection]
  (map (fn [v] (get v 1)) collection))

(defn titles [books]
  (map (fn [book] (:title book)) books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
    ))

(defn contains-duplicates? [a-seq]
  (not (== (count a-seq)(count (set a-seq)))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)) ))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (map :name (authors books)))

(defn author->string [author]
  (let [
    name (:name author)
    birth (:birth-year author)
    death (:death-year author)]
    (if (contains? author :birth-year)
      (str name " (" birth " - " death ")" )
      (str name)
      )
    ))

(defn authors->string [authors]
  (apply str (interpose " and " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book)) ))

(defn books->string [books]
  (let [
      num-books (count books)
    ]
    (if (= num-books 0)
    "No books."
    (str num-books " books. " (apply str (interpose ", " (map book->string books))))
    )))

(defn books-by-author [author books]
   (map :title (filter  (fn [book] (contains? (:authors book) author) )  books)) )

(defn author-by-name [name authors]
  ; might need to check if the result is empty so we can return nil instead of ()
  (filter (fn [author] (= name (:name author)) ) authors))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (filter alive? (:authors book)))) )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
