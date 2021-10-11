let e = HashSet.Int.empty

let insertTest = () => {
  let ar = Belt.Array.range(0, 16)
  let s1 = Belt.Array.reduce(ar, e, (s, i) => HashSet.Int.add(s, i))

  s1->HashSet.Int.log

  assert (s1->HashSet.Int.size == 17)

  // let s2 = Belt.Array.reduce(ar, s1, (s, i) => HashSet.Int.add(s, i))
  // assert (s2->HashSet.Int.size == 17)
}

let removeTest = () => {
  let ar = Belt.Array.range(0, 16)
  let s1 = Belt.Array.reduce(ar, e, (s, i) => HashSet.Int.add(s, i))
  let s2 =
    s1
    ->HashSet.Int.remove(0)
    ->HashSet.Int.remove(1)
    ->HashSet.Int.remove(2)
    ->HashSet.Int.remove(3)
    ->HashSet.Int.remove(4)
    ->HashSet.Int.remove(5)
    ->HashSet.Int.remove(6)
    ->HashSet.Int.remove(7)
    ->HashSet.Int.remove(8)
    ->HashSet.Int.remove(9)
  s2->HashSet.Int.log
}

let faillingTest = () => {
  let actions = [19, 33, 25, 37, 30, 3, 42, 22, 35, 23, 8, 9, 34, 17, 2, 27, 48, 43, 31, 49, 23]

  let s = Belt.Array.reduce(actions, e, (s, x) => {
    // s->HashSet.Int.log
    s->HashSet.Int.add(x)
  })
  // s->HashSet.Int.log

  assert (s->HashSet.Int.size == 20)
}

faillingTest()

let perfComparison = rep => {
  let repeat = (n, f) => {
    for _ in 1 to n {
      f()
    }
  }
  let a1k = Belt.Array.makeByAndShuffle(1000, i => i)

  %time(repeat(rep, () => HashSet.Int.fromArray(a1k)->ignore))->Js.log

  %time(repeat(rep, () => ImmutableJs.Set.fromArray(a1k)->ignore))->Js.log
}

perfComparison(1000)
