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
  
  pipeline :api_auth do 
    plug Guardian.Plug.Pipeline, module: Gites.Guardian,
                                 error_handler: GitesWeb.AuthErrorHandler
    plug Guardian.Plug.VerifyHeader, realm: "Bearer"
    plug Guardian.Plug.LoadResource
  end 


  scope "/api", GitesWeb do
    pipe_through :api
    
    post "/signup", UserController, :create
    post "/login", UserSessionController, :create
    resources "/bookings", BookingController, only: [:create]
    resources "/availabilities", AvailabilityController, only: [:index]
    resources "/pagesdata", PageDataController, only: [:show]
  end

  scope "/api/restricted", GitesWeb do 
    pipe_through [ :api, :api_auth ]
    get "/users", UserController, :index 
    get "/refresh_jwt", UserSessionController, :refresh
    get "/logout", UserSessionController, :delete
    post "/presigned_url", AwsUploadController, :get_url
    get "/list_bucket", AwsUploadController, :index
    get "/list_bucket/:folder", AwsUploadController, :show
    get "/delete_obj/:filename", AwsUploadController, :delete
    get "/delete_folder/:folder", AwsUploadController, :delete_folder
    post "/pagesdata", PageDataController, :create
    get "/availabilities", AvailabilityController, :index_admin
    delete "/availabilities/:id", AvailabilityController, :delete
    put "/availabilities", AvailabilityController, :create
    get "/bookings", BookingController, :index
    delete "/bookings/:id", BookingController, :delete
    put "/bookings", BookingController, :update

  end 

  scope "/", GitesWeb do
    pipe_through :browser

    get "/admin", AdminController, :index
    get "/*path", PageController, :index
  end

end
