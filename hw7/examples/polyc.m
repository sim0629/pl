let rec f = fn x => f f in f end
(* error (타입이 정의되지 않음) *)
