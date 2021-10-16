type rec node<'k, 'v> =
  | ArrayMap(arrayMapNode<'k, 'v>)
  | BitmapIndexed(bitmapIndexedNode<'k, 'v>)
  | HashArrayMap(hashArrayMapNode<'k, 'v>)
  | MapEntry(mapEntry<'k, 'v>)
  | HashCollision(hashCollisionNode<'k, 'v>)

and arrayMapNode<'k, 'v> = array<('k, 'v)>

and bitmapIndexedNode<'k, 'v> = {
  bitmap: int,
  data: array<node<'k, 'v>>,
}

and hashArrayMapNode<'k, 'v> = {
  count: int,
  nodes: array<option<node<'k, 'v>>>,
}

and mapEntry<'k, 'v> = ('k, 'v)

and hashCollisionNode<'k, 'v> = {
  hash: int,
  entries: array<mapEntry<'k, 'v>>,
}

let empty: unit => node<'k, 'v>

let find: (node<'k, 'v>, ~shift: int, ~hash: int, ~key: 'k) => option<'v>

let assoc: (
  node<'k, 'v>,
  ~shift: int,
  ~hasher: (. 'k) => int,
  ~hash: int,
  ~key: 'k,
  ~value: 'v,
) => option<node<'k, 'v>>


type dissocResult<'k, 'v> = Empty | Node(node<'k, 'v>)

/**
 * key 에 해당하는 값이 없으면 None 반환
 * Some(Empty) 일 경우 재귀적으로 Empty 여부를 체크해야 함
 * 
 * dissoc은 canonical form을 보장하지 않음 (예: MapEntry가 1개인 bitmapIndexedNode가 생길 수 있음)
 */
let dissoc: (node<'k, 'v>, ~shift: int, ~hash: int, ~key: 'k) => option<dissocResult<'k, 'v>>

let log: node<'k, 'v> => unit
