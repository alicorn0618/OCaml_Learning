let rec range ~first: a ~last: b =
  if a > b then []
  else a :: range ~first: (a + 1) ~last: b;;

