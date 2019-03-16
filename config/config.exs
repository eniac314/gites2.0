# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :gites,
  ecto_repos: [Gites.Repo]

# Configures the endpoint
config :gites, GitesWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "vkCAf/k7+5Vb26RESpKxEh5mX1jwmc3jNay5ZG1xPhlo9/n2BdTD2C/WygoQIRWf",
  render_errors: [view: GitesWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Gites.PubSub, adapter: Phoenix.PubSub.PG2]

# Configures guardian 
config :gites, Gites.Guardian,
  allowed_algos: ["HS512"],
  secret_key: System.get_env("GUARDIAN_SECRET_KEY"),

  issuer: "Gites",
  ttl: { 5, :minutes },
  serializer: Gites.Guardian


# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason


config :recaptcha,
    public_key: {:system, "RECAPTCHA_PUBLIC_KEY"},
    secret: {:system, "RECAPTCHA_PRIVATE_KEY"}

config :ex_aws,
  access_key_id:     System.get_env("AWS_ACCESS_KEY_ID"),
  secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY"),
  region: "eu-west-3",
  s3: [
    scheme: "https://",
    host: "s3.eu-west-3.amazonaws.com",
    region: "eu-west-3" ]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"


