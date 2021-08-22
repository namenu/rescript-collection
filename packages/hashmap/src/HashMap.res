type hasher<'k> = 'k => int

type t<'k, 'v> = {
  root: Hamt.t<'k, 'v>,
  count: int,
  hasher: hasher<'k>,
}

let make = (~hasher) => {
  root: Hamt.make(),
  count: 0,
  hasher: hasher,
}

let get = ({root, hasher}, k) => {
  Hamt.find(root, ~shift=0, ~hash=hasher(k), ~key=k)
}

let set = ({root, count, hasher} as m, k, v) => {
  let root' = Hamt.assoc(root, ~shift=0, ~hasher, ~hash=hasher(k), ~key=k, ~value=v)
  if root' === root {
    m
  } else {
    {
      ...m,
      root: root',
      count: count + 1,
    }
  }
}

let remove = ({root, count, hasher} as m, k) => {
  switch Hamt.dissoc(root, ~shift=0, ~hash=hasher(k), ~key=k) {
  | Some(root') =>
    if root' === root {
      m
    } else {
      {
        ...m,
        root: root',
        count: count - 1,
      }
    }

  | None => make(~hasher)
  }
}

let size = m => m.count
