defmodule Gites.Repo do
  use Ecto.Repo,
    otp_app: :gites,
    adapter: Ecto.Adapters.Postgres
end
