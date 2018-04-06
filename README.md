# snake-feet.lisp
snake-feet.lisp provide some useful iterator for consed list and vector sequence in commonlisp :D  
this package support some functions that same as popular one. `map`, `filter`, `reduce`, `slice` and etc.
if you want check supported functions, you can read the Methods topic.
abuot this iterators characteristic, each iterator elements don't evaluate until necessary, and don't make a new sequence everytimes. so some cases, you can saving memory. maybe :D 
(but ireverse and isort is slower than other methods, because those collect and cache iteration results.)
this package is not speedy, because I did not optimize it. if you want, you can find a better library than this one :D

```lisp
(defvar example (iterator '(1 2 3)))
(next example) ;; 1

(defvar copied (copy example))
(next example) ;; 2
(next example) ;; 3
(next example) ;; *stop-iteration*
(to-list (imap '+1 copied)) ;; (3 4)
```

## overview functions

### evaluation strategy
snake-feets iterator dont evaluate element until necessary.
in this case, actually range iterator and map iterator will not make and calculate 1000 continuous numbers. because slice iterator dont evaluate values that is out of range.

```lisp 
(to-array
  (islice 0 5
    (imap '1+
      (range 1000)))) ;; (1 2 3 4 5 6)
```

### iterator is copyable 
all iterators can copy with function `copy`  with the exception of some iterators (file iterator and function iterator, because those are cannot copy a status).
and copied iterator is independent to original iterator.
if you want copy a iterator that cannot be copied, you can use the cache iterator as proxy.

```lisp
(copy (ifile file)) ;; error 
(copy (icache (ifile file))) ;; success
```

### make an iterator from function
you can make a iterator from function if you want. 
for this example, made iterator could not stop, so it give value forever, but you can stop iteration with return a value of `*stop-iteration*` in the function.

```lisp
(iterator
  (lambda () "generated value!"))
```

### cache iterator
this package has supported cache iterator. it is useful. 
cache iterator record a value from iterator of argument and return it. but if can use  cache, this return a cached value. 
if this iterator was copied, it share a cache object with exception of position of read.
be careful, cache iterator consume memory too much than other iterator. 
because this iterator make a pool that for collect given value from source iterator.

```lisp
(let* 
  ((iter (icache (imap '1+ (range 0 1000))))
   (iter2 (copy iter)))
  (to-array iter) ;; (1 2 3 ... 1000)
  (to-array iter2)) ;; (1 2 3 ... 1000) its cached value
```

### snake-feet-mamba.lisp 
snake-feet-mamba.lisp is a optimized package. original package is not fast, because it used defclass and defmethod. so I wrote it from scratch without class defining for performance. so its very faster than original package.
it now developing, so it have not enough function and iterator that compare by original package. please wait for complete.

## Performance
I wrote simple code for performance check. you can read to [here](benchmark.lisp) if you want :D  
I show performance table on here. on current version (march 5th), snake-feet and snake-feet-mamba are too slow than mapcar and some native functions. I hope that those will be more faster.
but those saved memory too much than mapcar. of course, it is no wonder, because mapcar return a new sequence.

### CPU performance 
| code | processor cycles | processor cycles per mapcar |
---- | ---- | ---- 
| snake-feet | 523,144,764 | 845.05% |
| snake-feet-mamba | 105,921,236 | 171.09% |
| mapcar | **61,906,231** | **100%** |

### Memory performance 
| code | bytes consed | bytes consed per mapcar |
---- | ---- | ---- 
| snake-feet | 5,813,408 | 9.56% |
| snake-feet-mamba | **393,152** | **0.64%** |
| mapcar | 60,775,232 | 100% |

## Methods
### Basic Methods
| Function | Description |
---- | ----
| `(next iterator)` | get a value from iterator. and change iterators status to the next. |
| `(skip iterator)` | just change iterators status to the next. |
| `(copy iterator)` | copy an iterator and return. |
| `(iterator sequence-or-iterator)` | this function make a new iterator by argument. if argument is a sequential, make a new iterator by it by overloaded function. if argument is function, this function make a iterator by it. if argument is a iterator, this function just return it. |
| `(range start &optional end step)` | this function make a range iterator. range iterator give a continuous number in range. if give function one argument, this make a range iterator that return a number between 0 to first argument. if give function two arguments, this make a range itrator that return a number between `start` to `end`. last, if give function all arguments, this make a range iterator that return a number between `start` to `end`. everytimes, that number increase  by `step`. |
| `(repeat count &optional element)` | this function make a repeat iterator by arguments. the repeat iterator give a `element` until `count` times. |
| `(imap function iterator)` | this function make a map iterator by arguments. the map iterator give applied value by function. map iterator like as `(mapcar ...)`. |
| `(ifilter function iterator)` | this function make a filter iterator by arguments. the filter iterator give a value that trued by function from iterator. filter iterator like as `(filter ...)`. |
| `(islice start end iterator)` | this function make a slice iterator by arguments. the slice iterator give a element that position is between `start` to `end`. this function like as `(subseq ...)`. |
| `(iappend &rest iterators)` | this function make a append iterator by arguments. the append iterator give a value from multiple iterators. if first iterator was spended all, this switch to next iterator. |
| `(izip &rest iterators)` | this function make a zip iterator by arguments. the zip iterator give a consed sequence from owned iterators. this iterator will be end when one owned iterator was reached by end. | 
| `(istep offset step iterator)` | this function make a step iterator by arguments. the step iterator choose a element by designated interval, from iterator. |
| `(ireverse function iterator)` | this function make a reverse iterator by arguments. reverse iterator give a element by reversed order from source iterator. (*be careful!* this functions performance is not good. because reverse iterator collect and cache all elements from source iterator.) |
| `(isort function iterator)` | this function make a sort iterator by arguments. sort iterator give a element by sorted order by function, from source iterator. (*be careful!* this functions performance is not good. because sort iterator collect and cache all elements from source iterator.) | 
| `(ifile file &key read-function)` | this function return file iterator by arguments. file iterator give a charactor or octet from file stream. file iterator cannot copy. if you want copy the file iterator, you should use `(icache iterator)` as proxy. |
| `(icache iterator)` | this function make a cache iterator by arguments. cache iterator give and record a value from source iterator. cache iterator can use to proxy for source iterator that cannot copy self. |

### Summarize Methods
those functions dont make a new iterator. those functions spend iterator for returning value. so spended iterators will be changed status. if you dont want change the iterator status, you can use `(copy iterator)` method.

| Function | Description | 
---- | ---- 
| `(to-list iterator)` | make a new consed list from iterator. |
| `(to-array iterator)` | make a new array from iterator. |
| `(ievery function iterator)` | if all elements is true by conditional function, this function return `t`, otherwise return `nil`. |
| `(isome fnuction iterator)` | if a element is true by conditional function, this function return `t`, otherwise return `nil`. |
| `(ireduce function iterator)` | combine all elements of iterator by function. if iterator has not element, this return nil. if iterator has one element, this return it. this function is like as `(reduce ...)`. | 
| `(ifind-if function iterator)` | search a element by function from source iterator. |
| `(ifind item iterator)` | search a element by item from source iterator. |
| `(iposition-if function iterator)` | this function search a  index for matched by functions condition. if could not found index, this function return a nil. (this will return multiple value since next version). |
| `(iposition item iterator)` | this function return a index of element that match as item. if could not found index, this function return a nil. (this will return multiple value since next version). |
| `(icount-if function iterator)` | count element from iterator with trued by function result. |
| `(icount item iterator)` | count element from iterator that matched by argument. | 

## License 
this package released under the MIT Licese.
