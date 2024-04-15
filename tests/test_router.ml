open Helix
open Stdweb.Dom

module Date : sig
  type t

  val of_string : string -> t option
  val to_string : t -> string
  val equal : t -> t -> bool
end = struct
  type t = string

  let of_string str =
    match String.split_on_char '-' str with
    | [ yyyy; mm; dd ]
      when String.length yyyy = 4
           && String.length dd = 2
           && String.length mm = 2 -> Some str
    | _ -> None

  let to_string t = t
  let equal = String.equal
end

module Device_metrics = struct
  module Links = struct
    let date =
      Router.var ~of_string:Date.of_string ~to_string:Date.to_string
        ~equal:Date.equal "date"

    let index = Router.End
    let count = Router.Const ("count", End)
    let avg = Router.Const ("avg", Var (date, None, Var (date, None, End)))

    (* let avg = Router.Const ("avg", Var (date, Var (date, End))) *)
    let sum = Router.(Const ("sum", Var (int, None, Var (int, None, End))))
    (* let%path sum = ["sum"; int; int] *)
    (* let%path sum = "sum/:int/:int" *)
  end

  let view_sum x y () =
    let open Html in
    Signal.pair x y
    |> Signal.uniq ~equal:( = )
    |> Helix.show (fun (x, y) -> span [] [ text "SUM: "; int (x + y) ])

  let view router =
    let start_date = Signal.make (Option.get (Date.of_string "2023-01-01")) in
    let end_date = Signal.make (Option.get (Date.of_string "2023-04-01")) in
    let open Html in
    div []
      [
        div []
          [
            (*show (fun path -> code [] (String.concat "/" path)) (Router.path router);*)
            ul []
              [
                li []
                  [
                    a
                      [
                        Router.link
                          ~active:(style_list [ ("font-weight", "bold") ])
                          router Links.count;
                      ]
                      [ text "Count" ];
                  ];
                li []
                  [
                    text "Average from ";
                    input
                      [
                        type' "date";
                        bind
                          (fun date -> value (Date.to_string date))
                          start_date;
                        on Event.input (fun ev ->
                            let date =
                              Event.target ev
                              |> Node.get_value
                              |> Date.of_string
                              |> Option.get
                            in
                            Signal.emit date start_date
                        );
                      ];
                    text " to ";
                    input
                      [
                        type' "date";
                        bind (fun date -> value (Date.to_string date)) end_date;
                        on Event.input (fun ev ->
                            let date =
                              Event.target ev
                              |> Node.get_value
                              |> Date.of_string
                              |> Option.get
                            in
                            Signal.emit date end_date
                        );
                      ];
                    nbsp;
                    a
                      [
                        bind
                          (fun (start_date, end_date) ->
                            Router.link
                              ~active:(style_list [ ("font-weight", "bold") ])
                              router Links.avg start_date end_date
                          )
                          (Signal.pair start_date end_date);
                      ]
                      [ text "Go" ];
                  ];
                li []
                  [
                    a
                      [
                        Router.link
                          ~active:(style_list [ ("font-weight", "bold") ])
                          router Links.sum 2 3;
                      ]
                      [ text "Sum 2 3" ];
                  ];
                li []
                  [
                    a
                      [
                        Router.link
                          ~active:(style_list [ ("font-weight", "bold") ])
                          router Links.sum 100 1;
                      ]
                      [ text "Sum 100 1" ];
                  ];
              ];
            hr [];
          ];
        div
          [ style "background: #cef" ]
          [
            Router.dispatch ~label:"metrics" router
              [
                Router.route Links.index (fun () ->
                    Html.text "PICK METRIC ABOVE"
                );
                Router.route Links.count (fun () -> Html.text "COUNT");
                Router.route Links.avg (fun start_date end_date () ->
                    Html.span []
                      [
                        text "AVG for ";
                        show (fun date -> text (Date.to_string date)) start_date;
                        text " - ";
                        show (fun date -> text (Date.to_string date)) end_date;
                      ]
                );
                Router.route Links.sum view_sum;
              ];
          ];
      ]
end

module Device = struct
  module Links = struct
    let index = Router.End
    let metrics = Router.(Const ("metrics", Rest))
    let schema = Router.(Const ("schema", End))
  end

  let view (device_id : string signal) router =
    let open Html in
    div []
      [
        h2 [] [ text "Device" ];
        device_id
        |> show ~label:"device-header" (fun device_id ->
               div []
                 [
                   text ("device_id=" ^ device_id);
                   ul []
                     [
                       li []
                         [
                           a
                             [
                               Router.link
                                 ~active:(style_list [ ("font-weight", "bold") ])
                                 ~exact:true router Links.index;
                             ]
                             [ text "Overview" ];
                         ];
                       li []
                         [
                           a
                             [
                               Router.link router
                                 ~active:(style_list [ ("font-weight", "bold") ])
                                 Links.metrics Device_metrics.Links.index;
                             ]
                             [ text "Metrics" ];
                         ];
                       li []
                         [
                           a
                             [
                               Router.link
                                 ~active:(style_list [ ("font-weight", "bold") ])
                                 router Links.schema;
                             ]
                             [ text "Schema" ];
                         ];
                     ];
                   hr [];
                 ]
           );
        div
          [ style "background: #cef" ]
          [
            Router.dispatch ~label:"device" router
              [
                Router.route Links.index (fun () -> Html.text "OVERVIEW");
                Router.route Links.metrics Device_metrics.view;
                Router.route Links.schema (fun () -> Html.text "SCHEMA");
              ];
          ];
      ]
end

module Links = struct
  let root = Router.End

  let devices =
    let open Router in
    Const ("devices", Var (string, None, Rest))

  let devices_edit =
    let open Router in
    Const ("devices", Var (string, None, Const ("!edit", End)))

  let devices_new =
    let open Router in
    Const ("devices", Const ("!new", End))

  let device_schema_edit =
    let open Router in
    Const ("devices", Var (string, None, Const ("schema", (Const ("!edit", End)))))

  let account =
    let open Router in
    Const ("account", End)
end

let view router =
  let open Html in
  div []
    [
      h1 [] [ text "INDEX" ];
      pre []
        [
          show
            (fun parts -> text ("/" ^ String.concat "/" parts))
            (Router.path router);
        ];
      hr [];
      ul []
        [
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    ~exact:true Links.root;
                ]
                [ text "#/" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.account;
                ]
                [ text "#/account" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_1" Device.Links.index;
                ]
                [ text "#/devices/dev_1" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_3" Device.Links.index;
                ]
                [ text "#/devices/dev_3" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_1" Device.Links.metrics
                    Device_metrics.Links.index;
                ]
                [ text "#/devices/dev_1/metrics" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_1" Device.Links.schema;
                ]
                [ text "#/devices/dev_1/schema" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_2" Device.Links.schema;
                ]
                [ text "#/devices/dev_2/schema" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices "dev_2" Device.Links.metrics
                    Device_metrics.Links.index;
                ]
                [ text "#/devices/dev_2/metrics" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices_edit "dev_2";
                ]
                [ text "#/devices/dev_2/!edit" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.devices_new;
                ]
                [ text "#/devices/!new" ];
            ];
          li []
            [
              a
                [
                  Router.link router
                    ~active:(style_list [ ("font-weight", "bold") ])
                    Links.device_schema_edit "dev_1";
                ]
                [ text "#/devices/dev_1/schema/!edit" ];
            ];
        ];
      hr [];
      Router.dispatch router ~label:"main" ~default:(text "NOT FOUND")
        [
          Router.route Links.root (fun () -> Html.text "ROOT");
          Router.route Links.account (fun () -> Html.text "ACCOUNT");
          Router.route Links.devices Device.view;
          Router.route Links.devices_edit (fun id () ->
              show (fun id -> Html.text ("DEVICE EDIT: " ^ id)) id
          );
          Router.route Links.devices_new (fun () -> Html.text "DEVICE NEW");
          Router.route Links.device_schema_edit (fun dev_id () -> let$ dev_id in Html.text  ("DEVICE SCHEMA EDIT: " ^ dev_id));
        ];
    ]

let () =
  let router = Router.make History.hash_path in
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some node -> Html.mount node (view router)
  | None -> failwith "no #root node"
