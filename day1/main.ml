open Core

let first_last_num line =
  let digits = String.filter ~f:Char.is_digit line in
  match String.to_list digits with
  | [] -> None
  | [x] -> Some (x, x)
  | first :: rest -> 
      let last = List.last_exn rest in
      Some (first, last)

let read_lines file =
  let content = In_channel.read_all file in
  let lines = String.split_lines content in
  let tuples = List.filter_map ~f:first_last_num lines in
  tuples

let sum = ref 0

let part1 (x, y) =
  let str = Printf.sprintf "%c%c" x y in
  let num = (Int.of_string str) in
  sum := !sum + num;
  printf "%d\n" !sum

let () =
  let result = read_lines "input.txt" in
  List.iter ~f:part1 result
