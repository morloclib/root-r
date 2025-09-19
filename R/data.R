# _unpack_map R :: unpack => Map a b -> ([a],[b]);
morloc_unpack_map <- function(x){
  list(names(x), unname(x))
}

# _pack_map :: pack => ([a],[b]) -> Map a b;
morloc_pack_map <- function(xs){
  names(xs[[2]]) <- xs[[1]]  
  as.list(xs[[2]])
}

morloc_packUnit <- function(x) {
  NULL
}

morloc_unpackUnit <- function(x){
  1
}
