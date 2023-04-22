open Zora

module A = Belt.Array
module V = Vector

let isomorphic = ar => ar->V.fromArray->V.toArray == ar
let pushpop = (n, m) => {
  let v = V.fromArray(A.range(1, n))
  A.reduce(A.range(1, m), v, (v, _) => v->V.pop)
}

zora("Vector initialize", async t => {
  t->equal(V.make()->V.length, 0, "make empty vector")
  t->equal(V.fromArray([]), V.make(), "make from empty array")

  A.range(1, 32)->A.forEach(n => {
    t->ok(isomorphic(A.range(1, n)), `fromArray length=${n->Belt.Int.toString}`)
  })

  A.rangeBy(1000, 10000, ~step=1000)->A.forEach(n => {
    t->ok(isomorphic(A.range(1, n)), `fromArray length=${n->Belt.Int.toString}`)
  })
})

zora("Vector.push", async t => {
  t->test("push", async t => {
    A.range(1, 64)->A.forEach(
      n => {
        let v1 = A.reduce(A.range(1, n), V.make(), (v, i) => V.push(v, i))
        let v2 = A.range(1, n)->V.fromArray
        t->equal(v1, v2, "should be equal")
      },
    )
  })

  t->test("root overflow", async t => {
    let n = 32768
    let v1 = A.reduce(A.range(1, n), V.make(), (v, i) => V.push(v, i))
    let v2 = A.range(1, n)->V.fromArray

    t->equal(v1, v2, "should be equal")
  })
})

zora("Vector.pop", async t => {
  t->test("pop", async t => {
    [(100, 50), (100, 100), (10000, 5000)]->A.forEach(
      ((n, m)) => {
        t->equal(pushpop(n, m)->V.toArray, A.range(1, n - m), "should be equal")
      },
    )
  })

  t->test("root overflow", async t => {
    let ar = A.range(1, 32768)
    let v = V.fromArray(ar)
    let ev = A.reduce(ar, v, (v, _) => V.pop(v))

    t->ok(ev->V.length == 0, "should be empty")
  })
})

zora("Vector.get", async t => {
  let v = pushpop(20000, 10000)
  t->block("random access (10,000 times)", t => {
    let every = A.every(
      A.range(1, 10000),
      _ => {
        let idx = Js.Math.random_int(0, 10000)
        V.getExn(v, idx) == idx + 1
      },
    )
    t->ok(every, "should be succeed")
  })

  t->block("tail offset 0", t => {
    let v = V.push(V.make(), 1)
    let v' = V.setUnsafe(v, 0, 2)
    t->is(V.getUnsafe(v', 0), 2, "should be 2")
  })

  t->block("optional get", t => {
    [-1, 0, 10000]->A.forEach(
      idx => {
        switch V.get(v, idx) {
        | Some(_) => t->ok(idx >= 0 && idx < V.length(v), "should be ok")
        | None => t->notOk(idx >= 0 && idx < V.length(v), "should not be ok")
        }
      },
    )
  })

  t->block("out of bounds", t => {
    t->optionNone(V.get(v, -1), "should be none")
    t->optionNone(V.get(v, 10000), "should be none")
  })
})

zora("Vector.set", async t => {
  let size = 10000
  let v = V.fromArray(A.range(1, size))

  t->block(`random update (${size->Belt.Int.toString} items)`, t => {
    let ar = A.range(1, size)->A.shuffle
    let v' = A.reduce(ar, v, (v, idx) => V.setExn(v, idx - 1, idx * -1))
    let every = A.every(v'->V.toArray, x => x < 0)

    t->ok(every, "shoud be ok")
  })

  t->block("optional set", t => {
    [-1, 0, 10000]->A.forEach(
      idx => {
        switch V.set(v, idx, 42) {
        | Some(_) => t->ok(idx >= 0 && idx < V.length(v), "should be ok")
        | None => t->notOk(idx >= 0 && idx < V.length(v), "should not be ok")
        }
      },
    )
  })

  let ar = A.range(1, size)
  t->block(`mutable random update (${size->Belt.Int.toString} times)`, t => {
    A.forEach(A.range(1, size)->A.shuffle, idx => A.setUnsafe(ar, idx - 1, idx * -1))
    let every = A.every(ar, x => x < 0)
    t->ok(every, "should be ok")
  })
})

zora("Vector.reduce", async t => {
  let size = 100
  let v = V.fromArray(A.range(1, size))

  t->block("sum", t => {
    let sum = V.reduce(v, 0, (acc, i) => acc + i)
    t->is(sum, 5050, "sum is 5050")
  })

  t->block("sum (uncurried)", t => {
    let sum = V.reduceU(v, 0, (. acc, i) => acc + i)
    t->is(sum, 5050, "sum is 5050")
  })
})

zora("Prop test: should always equally sized", async t => {
  open FastCheck
  open Arbitrary
  open Property.Sync
  let p1 = Combinators.arrayWithLength(integer(), 0, 10000)
  let p2 = integerRange(2, 10) // push probability (10% ~ 50%)
  assert_(
    property2(p1, p2, (xs, prob) => {
      let a: array<int> = []
      let v: ref<V.t<int>> = ref(V.make())
      A.forEach(
        xs,
        n => {
          v := if mod(n, prob) != 0 {
              Js.Array2.push(a, n)->ignore
              V.push(v.contents, n)
            } else {
              Js.Array2.pop(a)->ignore
              V.pop(v.contents)
            }

          assert (a == V.toArray(v.contents))
          assert (A.length(a) == V.length(v.contents))
        },
      )

      t->equal(A.length(a), V.length(v.contents), "equal length")
      A.length(a) == V.length(v.contents)
    }),
  )
})
