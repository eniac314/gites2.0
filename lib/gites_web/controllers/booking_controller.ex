defmodule GitesWeb.BookingController do
  use GitesWeb, :controller

  plug(Guardian.Plug.EnsureAuthenticated when action in [:delete, :update, :index])

  alias Gites.BookingSystem
  alias Gites.BookingSystem.Booking
  alias Gites.Mailer
  alias Gites.Email

  action_fallback(GitesWeb.FallbackController)

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

    with {:ok, _response} <- Recaptcha.verify(booking_params["captcha_response"]),
         {:ok, %Booking{} = booking} <- BookingSystem.create_booking(booking_params),
         {:ok, _res} <-
           BookingSystem.bulk_create_availabilities(booking.id, booking_params["days_booked"]) do
      GitesWeb.Endpoint.broadcast!("bookings:locked_days", "new_booking", %{})

      Email.notif_email(
        booking_params["email"],
        booking_params["notification_mail"]["subject"],
        booking_params["notification_mail"]["body"]
      )
      |> Mailer.deliver_later()

      Email.notif_admin_email(
        booking_params["notification_mail_admin"]["subject"],
        booking_params["notification_mail_admin"]["body"]
      )
      |> Mailer.deliver_later()

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

  def update(conn, %{
        "booking" => booking_params,
        "reply_address" => dest,
        "confirmation_email" => mail
      }) do
    serialized_options = Poison.encode!(booking_params["options"])
    booking_params = Map.put(booking_params, "options", serialized_options)

    booking = BookingSystem.get_booking!(booking_params["bookingId"])

    with {:ok, %Booking{} = _booking} <- BookingSystem.update_booking(booking, booking_params) do
      Email.notif_email(dest, mail["subject"], mail["body"])
      |> Mailer.deliver_later()

      send_resp(conn, :no_content, "")
    end
  end

  def delete(conn, %{"id" => id}) do
    booking = BookingSystem.get_booking!(id)

    with {:ok, %Booking{}} <- BookingSystem.delete_booking(booking) do
      GitesWeb.Endpoint.broadcast!("bookings:locked_days", "need_refresh", %{})
      send_resp(conn, :no_content, "")
    end
  end
end
