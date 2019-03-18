defmodule GitesWeb.BookingController do
  use GitesWeb, :controller

  alias Gites.BookingSystem
  alias Gites.BookingSystem.Booking

  # require IEx

  action_fallback GitesWeb.FallbackController

  def index(conn, _params) do
    bookings = BookingSystem.list_bookings()
    render(conn, "index.json", bookings: bookings)
  end

  def create(conn, %{"booking" => booking_params}) do
    # IEx.pry()
    with  {:ok, _response} <- Recaptcha.verify(booking_params["captcha_response"]),
          {:ok, %Booking{} = booking} <- BookingSystem.create_booking(booking_params),
          {:ok, _res } <- BookingSystem.bulk_create_availabilities(booking_params["days_booked"]) do          
      conn
      |> put_status(:created)
      |> put_resp_header("location", Routes.booking_path(conn, :show, booking))
      |> render("booking_success.json", booking: booking)
    end
  end

  def show(conn, %{"id" => id}) do
    booking = BookingSystem.get_booking!(id)
    render(conn, "show.json", booking: booking)
  end

  def update(conn, %{"id" => id, "booking" => booking_params}) do
    booking = BookingSystem.get_booking!(id)

    with {:ok, %Booking{} = booking} <- BookingSystem.update_booking(booking, booking_params) do
      render(conn, "show.json", booking: booking)
    end
  end

  def delete(conn, %{"id" => id}) do
    booking = BookingSystem.get_booking!(id)

    with {:ok, %Booking{}} <- BookingSystem.delete_booking(booking) do
      send_resp(conn, :no_content, "")
    end
  end

end
