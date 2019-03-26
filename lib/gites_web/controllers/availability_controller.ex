defmodule GitesWeb.AvailabilityController do
  use GitesWeb, :controller
  
  plug Guardian.Plug.EnsureAuthenticated when action in [:index_admin, :create, :delete]

  alias Gites.BookingSystem
  alias Gites.BookingSystem.Availability

  action_fallback GitesWeb.FallbackController

  def index(conn, _params) do
    availabilities = BookingSystem.list_availabilities()
    render(conn, "index.json", availabilities: availabilities)
  end

  def index_admin(conn, _params) do
    availabilities = BookingSystem.list_availabilities()
    render(conn, "index_admin.json", availabilities: availabilities)
  end

  def create(conn, %{"availability" => availability_params}) do
    with {:ok, %Availability{} = availability} <- BookingSystem.create_availability(availability_params) do
      GitesWeb.Endpoint.broadcast!("bookings:locked_days", "need_refresh", %{})
      conn
      |> put_status(:created)
      |> render("show.json", availability: availability)
    end
  end

  def show(conn, %{"id" => id}) do
    availability = BookingSystem.get_availability!(id)
    render(conn, "show.json", availability: availability)
  end

  def update(conn, %{"id" => id, "availability" => availability_params}) do
    availability = BookingSystem.get_availability!(id)

    with {:ok, %Availability{} = availability} <- BookingSystem.update_availability(availability, availability_params) do
      render(conn, "show.json", availability: availability)
    end
  end

  def delete(conn, %{"id" => id}) do
    availability = BookingSystem.get_availability_by_date!(id)

    with {:ok, %Availability{}} <- BookingSystem.delete_availability(availability) do
      GitesWeb.Endpoint.broadcast!("bookings:locked_days", "need_refresh", %{})
      send_resp(conn, :no_content, "")
    end
  end
end
