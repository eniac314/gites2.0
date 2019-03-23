defmodule GitesWeb.BookingController do
  use GitesWeb, :controller
  
  plug Guardian.Plug.EnsureAuthenticated when action in [:delete, :update]


  alias Gites.BookingSystem
  alias Gites.BookingSystem.Booking
  alias Gites.Mailer 
  alias Gites.Email
  alias Gites.LockedAvailabilitiesServer
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
          {:ok, _res } <- BookingSystem.bulk_create_availabilities(booking.id, booking_params["days_booked"]) do          
      
      GitesWeb.Endpoint.broadcast!("bookings:locked_days", "new_booking", %{})
      
      Email.confirm_email(booking_params["email"]) |> Mailer.deliver_later

      conn
      |> put_status(:created)
      |> render("booking_success.json", booking: booking)
    end
  end

  def show(conn, %{"id" => id}) do
    booking = BookingSystem.get_booking!(id)
    render(conn, "show.json", booking: booking)
  end

  def update(conn, %{"booking" => booking_params}) do
    booking = BookingSystem.get_booking!(booking_params["bookingId"])

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
