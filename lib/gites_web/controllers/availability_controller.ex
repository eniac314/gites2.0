defmodule GitesWeb.AvailabilityController do
  use GitesWeb, :controller

  alias Gites.BookingSystem
  alias Gites.BookingSystem.Availability

  action_fallback GitesWeb.FallbackController

  def index(conn, _params) do
    availabilities = BookingSystem.list_availabilities()
    render(conn, "index.json", availabilities: availabilities)
  end

  # def create(conn, %{"availability" => availability_params}) do
  #   with {:ok, %Availability{} = availability} <- BookingSystem.create_availability(availability_params) do
  #     conn
  #     |> put_status(:created)
  #     |> put_resp_header("location", Routes.availability_path(conn, :show, availability))
  #     |> render("show.json", availability: availability)
  #   end
  # end

  # def show(conn, %{"id" => id}) do
  #   availability = BookingSystem.get_availability!(id)
  #   render(conn, "show.json", availability: availability)
  # end

  # def update(conn, %{"id" => id, "availability" => availability_params}) do
  #   availability = BookingSystem.get_availability!(id)

  #   with {:ok, %Availability{} = availability} <- BookingSystem.update_availability(availability, availability_params) do
  #     render(conn, "show.json", availability: availability)
  #   end
  # end

  # def delete(conn, %{"id" => id}) do
  #   availability = BookingSystem.get_availability!(id)

  #   with {:ok, %Availability{}} <- BookingSystem.delete_availability(availability) do
  #     send_resp(conn, :no_content, "")
  #   end
  # end
end
