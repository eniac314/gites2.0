defmodule GitesWeb.BookingControllerTest do
  use GitesWeb.ConnCase

  alias Gites.BookingSystem
  alias Gites.BookingSystem.Booking

  @create_attrs %{
    add_address: "some add_address",
    address: "some address",
    check_in: 42,
    check_out: 42,
    city: "some city",
    comments: "some comments",
    confirmed: true,
    country: "some country",
    email: "some email",
    first_name: "some first_name",
    last_name: "some last_name",
    nbr_adults: 42,
    nbr_children: 42,
    phone1: "some phone1",
    phone2: "some phone2",
    postcode: 42,
    title: "some title"
  }
  @update_attrs %{
    add_address: "some updated add_address",
    address: "some updated address",
    check_in: 43,
    check_out: 43,
    city: "some updated city",
    comments: "some updated comments",
    confirmed: false,
    country: "some updated country",
    email: "some updated email",
    first_name: "some updated first_name",
    last_name: "some updated last_name",
    nbr_adults: 43,
    nbr_children: 43,
    phone1: "some updated phone1",
    phone2: "some updated phone2",
    postcode: 43,
    title: "some updated title"
  }
  @invalid_attrs %{add_address: nil, address: nil, check_in: nil, check_out: nil, city: nil, comments: nil, confirmed: nil, country: nil, email: nil, first_name: nil, last_name: nil, nbr_adults: nil, nbr_children: nil, phone1: nil, phone2: nil, postcode: nil, title: nil}

  def fixture(:booking) do
    {:ok, booking} = BookingSystem.create_booking(@create_attrs)
    booking
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all bookings", %{conn: conn} do
      conn = get(conn, Routes.booking_path(conn, :index))
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create booking" do
    test "renders booking when data is valid", %{conn: conn} do
      conn = post(conn, Routes.booking_path(conn, :create), booking: @create_attrs)
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get(conn, Routes.booking_path(conn, :show, id))

      assert %{
               "id" => id,
               "add_address" => "some add_address",
               "address" => "some address",
               "check_in" => 42,
               "check_out" => 42,
               "city" => "some city",
               "comments" => "some comments",
               "confirmed" => true,
               "country" => "some country",
               "email" => "some email",
               "first_name" => "some first_name",
               "last_name" => "some last_name",
               "nbr_adults" => 42,
               "nbr_children" => 42,
               "phone1" => "some phone1",
               "phone2" => "some phone2",
               "postcode" => 42,
               "title" => "some title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post(conn, Routes.booking_path(conn, :create), booking: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update booking" do
    setup [:create_booking]

    test "renders booking when data is valid", %{conn: conn, booking: %Booking{id: id} = booking} do
      conn = put(conn, Routes.booking_path(conn, :update, booking), booking: @update_attrs)
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get(conn, Routes.booking_path(conn, :show, id))

      assert %{
               "id" => id,
               "add_address" => "some updated add_address",
               "address" => "some updated address",
               "check_in" => 43,
               "check_out" => 43,
               "city" => "some updated city",
               "comments" => "some updated comments",
               "confirmed" => false,
               "country" => "some updated country",
               "email" => "some updated email",
               "first_name" => "some updated first_name",
               "last_name" => "some updated last_name",
               "nbr_adults" => 43,
               "nbr_children" => 43,
               "phone1" => "some updated phone1",
               "phone2" => "some updated phone2",
               "postcode" => 43,
               "title" => "some updated title"
             } = json_response(conn, 200)["data"]
    end

    test "renders errors when data is invalid", %{conn: conn, booking: booking} do
      conn = put(conn, Routes.booking_path(conn, :update, booking), booking: @invalid_attrs)
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete booking" do
    setup [:create_booking]

    test "deletes chosen booking", %{conn: conn, booking: booking} do
      conn = delete(conn, Routes.booking_path(conn, :delete, booking))
      assert response(conn, 204)

      assert_error_sent 404, fn ->
        get(conn, Routes.booking_path(conn, :show, booking))
      end
    end
  end

  defp create_booking(_) do
    booking = fixture(:booking)
    {:ok, booking: booking}
  end
end
