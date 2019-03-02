defmodule GitesWeb.Router do
  use GitesWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end
  

  scope "/", GitesWeb do
    pipe_through :browser

    get "/", PageController, :index
    get "/admin", AdminController, :index
  end

  scope "/api", GitesWeb do
    pipe_through :api

    get "/users", UserController, :index 
    post "/signup", UserController, :create
  end
end
