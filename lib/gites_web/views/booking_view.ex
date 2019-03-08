defmodule GitesWeb.BookingView do
  use GitesWeb, :view
  alias GitesWeb.BookingView

  def render("index.json", %{bookings: bookings}) do
    %{data: render_many(bookings, BookingView, "booking.json")}
  end

  def render("show.json", %{booking: booking}) do
    %{data: render_one(booking, BookingView, "booking.json")}
  end

  def render("booking_success.json", %{booking: _booking}) do 
    %{message: "booking_successfull"}
  end

  def render("booking.json", %{booking: booking}) do
    %{id: booking.id,
      check_in: booking.check_in,
      check_out: booking.check_out,
      title: booking.title,
      first_name: booking.first_name,
      last_name: booking.last_name,
      address: booking.address,
      add_address: booking.add_address,
      postcode: booking.postcode,
      city: booking.city,
      country: booking.country,
      phone1: booking.phone1,
      phone2: booking.phone2,
      email: booking.email,
      nbr_adults: booking.nbr_adults,
      nbr_children: booking.nbr_children,
      comments: booking.comments,
      confirmed: booking.confirmed}
  end
end
