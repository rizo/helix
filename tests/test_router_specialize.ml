open Helix
open Stdweb.Dom

module Links = struct
  open Helix.Router

  let team = Const ("teams", Var (string, None, Rest))

  let team_sig team_id_sig =
    Const ("teams", Var (string, Some team_id_sig, Rest))

  module Team = struct
    let projects = Const ("projects", End)
    let projects_new = Const ("projects", Const ("!new", End))

    let project_image_upload =
      Const
        ("projects", Var (string, None, Const ("images", Const ("!new", End))))

    let project_deployment =
      Const
        ( "projects",
          Var (string, None, Const ("deployments", Var (string, None, Rest)))
        )

    let project = Const ("projects", Var (string, None, Rest))

    module Project = struct
      let builds = Const ("builds", End)
      let images = Const ("images", End)
    end
  end
end

module Deployment_view = struct
  let make ~team_id:_ project_id deployment_id _deployment_router =
    let open Html in
    let$ project_id and$ deployment_id in
    text ("DEPLOYMENT: " ^ project_id ^ "/" ^ deployment_id)
end

module Project_list_view = struct
  let make ~team_id:_ team_router () =
    let open Html in
    div []
      [
        h3 [] [ text "PROJECT LIST" ];
        a
          [ Router.link team_router Links.Team.project "project_1" End ]
          [ text "project_1" ];
        br [];
        a
          [ Router.link team_router Links.Team.project "project_2" End ]
          [ text "project_2" ];
      ]
end

module Project_new_view = struct
  let make ~team_id:_ _team_router () =
    let open Html in
    text "PROJECT NEW"
end

module Image_upload_view = struct
  let make ~team_id:_ _team_router project_id () =
    let open Html in
    let$ project_id in
    text ("IMAGE UPLOAD: " ^ project_id)
end

module Project_view = struct
  let make ~team_id:_ project_id project_router =
    let open Html in
    let open Html in
    div []
      [
        h3 []
          [
            (let$ project_id in
             text ("PROJECT: " ^ project_id)
            );
          ];
        Router.dispatch ~label:"project" project_router
          Router.
            [
              route End (fun () -> text "PROJECT INDEX");
              route Links.Team.Project.builds (fun () -> text "BUILDS");
              route Links.Team.Project.images (fun () -> text "IMAGES");
            ];
      ]
end

module Team_view = struct
  let make team_id team_router =
    let open Html in
    div []
      [
        h2 []
          [
            (let$ team_id in
             text ("TEAM: " ^ team_id)
            );
          ];
        Router.dispatch ~label:"team" team_router
          Router.
            [
              route Links.Team.projects
                (Project_list_view.make ~team_id team_router);
              route Links.Team.projects_new
                (Project_new_view.make ~team_id team_router);
              route Links.Team.project (Project_view.make ~team_id);
              route Links.Team.project_image_upload
                (Image_upload_view.make ~team_id team_router);
              route Links.Team.project_deployment (Deployment_view.make ~team_id);
            ];
      ]
end

let view router =
  let current_team_id = Signal.make "team_1" in
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
      a [ href "#/" ] [ text "#/" ];
      br [];
      a
        [ href "#/teams/team_1/projects/project_1" ]
        [ text "#/teams/team_1/projects/project_1" ];
      br [];
      a
        [ href "#/teams/team_1/projects/project_2" ]
        [ text "#/teams/team_1/projects/project_2" ];
      br [];
      a
        [ href "#/teams/team_1/projects/!new" ]
        [ text "#/teams/team_1/projects/!new" ];
      br [];
      a
        [ href "#/teams/team_2/projects/project_3" ]
        [ text "#/teams/team_2/projects/project_3" ];
      br [];
      a
        [ href "#/teams/team_1/projects/project_1/deployments/dep_1" ]
        [ text "#/teams/team_1/projects/project_1/deployments/dep_1" ];
      br [];
      a
        [ href "#/teams/team_1/projects/project_1/images" ]
        [ text "#/teams/team_1/projects/project_1/images" ];
      br [];
      a
        [ href "#/teams/team_1/projects/project_1/images/!new" ]
        [ text "#/teams/team_1/projects/project_1/images/!new" ];
      hr [];
      Router.dispatch ~label:"main" router
        [
          Router.alias Router.End Links.team
            (Signal.get current_team_id)
            Links.Team.projects;
          Router.route (Links.team_sig current_team_id) Team_view.make;
        ];
    ]

let () =
  Helix.enable_debug false;
  let router = Router.make History.hash_path in
  match Stdweb.Dom.Document.get_element_by_id "root" with
  | Some node -> Html.mount node (view router)
  | None -> failwith "no #root node"
