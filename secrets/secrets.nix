let
  sammy-at-tau-19 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILCGvFKQqGkSt023z2AwXUO0mg3QuPnlwhYa7TbMJ0yH";
  users = [sammy-at-tau-19];

  muhbaasu-srv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ+zF8bNeCt7e7ReqEFgsYLK7hWNRo0jgRx7qNrZuj0K";

  systems = [muhbaasu-srv];
in {
  "grafana-secrets.age".publicKeys = users ++ systems;
}
