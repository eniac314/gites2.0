defmodule GitesWeb.BookingController do
  use GitesWeb, :controller
  
  plug Guardian.Plug.EnsureAuthenticated when action in [:delete, :update, :index]


  alias Gites.BookingSystem
  alias Gites.BookingSystem.Booking
  alias Gites.Mailer 
  alias Gites.Email
  alias Gites.LockedAvailabilitiesServer
  
  action_fallback GitesWeb.FallbackController

  def index(conn, _params) do
    bookings =
      BookingSystem.list_bookings()
      |> Enum.map(fn b ->
        %{
          b
          | options:
              if b.options do
                Poison.decode!(b.options)
              else
                nil
              end
        }
      end)

    render(conn, "index.json", bookings: bookings)
  end

  def create(conn, %{"booking" => booking_params}) do
    serialized_options = Poison.encode!(booking_params["options"]) 
    booking_params = Map.put(booking_params, "options", serialized_options)
    
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
    booking =
      BookingSystem.get_booking!(id)
      |> (fn b ->
            %{
              b
              | options:
                  if b.options do
                    Poison.decode!(b.options)
                  else
                    nil
                  end
            }
          end).()

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
