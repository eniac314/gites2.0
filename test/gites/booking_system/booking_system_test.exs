defmodule Gites.BookingSystemTest do
  use Gites.DataCase

  alias Gites.BookingSystem

  describe "bookings" do
    alias Gites.BookingSystem.Booking

    @valid_attrs %{add_address: "some add_address", address: "some address", check_in: 42, check_out: 42, city: "some city", comments: "some comments", confirmed: true, country: "some country", email: "some email", first_name: "some first_name", last_name: "some last_name", nbr_adults: 42, nbr_children: 42, phone1: "some phone1", phone2: "some phone2", postcode: 42, title: "some title"}
    @update_attrs %{add_address: "some updated add_address", address: "some updated address", check_in: 43, check_out: 43, city: "some updated city", comments: "some updated comments", confirmed: false, country: "some updated country", email: "some updated email", first_name: "some updated first_name", last_name: "some updated last_name", nbr_adults: 43, nbr_children: 43, phone1: "some updated phone1", phone2: "some updated phone2", postcode: 43, title: "some updated title"}
    @invalid_attrs %{add_address: nil, address: nil, check_in: nil, check_out: nil, city: nil, comments: nil, confirmed: nil, country: nil, email: nil, first_name: nil, last_name: nil, nbr_adults: nil, nbr_children: nil, phone1: nil, phone2: nil, postcode: nil, title: nil}

    def booking_fixture(attrs \\ %{}) do
      {:ok, booking} =
        attrs
        |> Enum.into(@valid_attrs)
        |> BookingSystem.create_booking()

      booking
    end

    test "list_bookings/0 returns all bookings" do
      booking = booking_fixture()
      assert BookingSystem.list_bookings() == [booking]
    end

    test "get_booking!/1 returns the booking with given id" do
      booking = booking_fixture()
      assert BookingSystem.get_booking!(booking.id) == booking
    end

    test "create_booking/1 with valid data creates a booking" do
      assert {:ok, %Booking{} = booking} = BookingSystem.create_booking(@valid_attrs)
      assert booking.add_address == "some add_address"
      assert booking.address == "some address"
      assert booking.check_in == 42
      assert booking.check_out == 42
      assert booking.city == "some city"
      assert booking.comments == "some comments"
      assert booking.confirmed == true
      assert booking.country == "some country"
      assert booking.email == "some email"
      assert booking.first_name == "some first_name"
      assert booking.last_name == "some last_name"
      assert booking.nbr_adults == 42
      assert booking.nbr_children == 42
      assert booking.phone1 == "some phone1"
      assert booking.phone2 == "some phone2"
      assert booking.postcode == 42
      assert booking.title == "some title"
    end

    test "create_booking/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = BookingSystem.create_booking(@invalid_attrs)
    end

    test "update_booking/2 with valid data updates the booking" do
      booking = booking_fixture()
      assert {:ok, %Booking{} = booking} = BookingSystem.update_booking(booking, @update_attrs)
      assert booking.add_address == "some updated add_address"
      assert booking.address == "some updated address"
      assert booking.check_in == 43
      assert booking.check_out == 43
      assert booking.city == "some updated city"
      assert booking.comments == "some updated comments"
      assert booking.confirmed == false
      assert booking.country == "some updated country"
      assert booking.email == "some updated email"
      assert booking.first_name == "some updated first_name"
      assert booking.last_name == "some updated last_name"
      assert booking.nbr_adults == 43
      assert booking.nbr_children == 43
      assert booking.phone1 == "some updated phone1"
      assert booking.phone2 == "some updated phone2"
      assert booking.postcode == 43
      assert booking.title == "some updated title"
    end

    test "update_booking/2 with invalid data returns error changeset" do
      booking = booking_fixture()
      assert {:error, %Ecto.Changeset{}} = BookingSystem.update_booking(booking, @invalid_attrs)
      assert booking == BookingSystem.get_booking!(booking.id)
    end

    test "delete_booking/1 deletes the booking" do
      booking = booking_fixture()
      assert {:ok, %Booking{}} = BookingSystem.delete_booking(booking)
      assert_raise Ecto.NoResultsError, fn -> BookingSystem.get_booking!(booking.id) end
    end

    test "change_booking/1 returns a booking changeset" do
      booking = booking_fixture()
      assert %Ecto.Changeset{} = BookingSystem.change_booking(booking)
    end
  end

  describe "availabilities" do
    alias Gites.BookingSystem.Availability

    @valid_attrs %{availability: "some availability", date: 42}
    @update_attrs %{availability: "some updated availability", date: 43}
    @invalid_attrs %{availability: nil, date: nil}

    def availability_fixture(attrs \\ %{}) do
      {:ok, availability} =
        attrs
        |> Enum.into(@valid_attrs)
        |> BookingSystem.create_availability()

      availability
    end

    test "list_availabilities/0 returns all availabilities" do
      availability = availability_fixture()
      assert BookingSystem.list_availabilities() == [availability]
    end

    test "get_availability!/1 returns the availability with given id" do
      availability = availability_fixture()
      assert BookingSystem.get_availability!(availability.id) == availability
    end

    test "create_availability/1 with valid data creates a availability" do
      assert {:ok, %Availability{} = availability} = BookingSystem.create_availability(@valid_attrs)
      assert availability.availability == "some availability"
      assert availability.date == 42
    end

    test "create_availability/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = BookingSystem.create_availability(@invalid_attrs)
    end

    test "update_availability/2 with valid data updates the availability" do
      availability = availability_fixture()
      assert {:ok, %Availability{} = availability} = BookingSystem.update_availability(availability, @update_attrs)
      assert availability.availability == "some updated availability"
      assert availability.date == 43
    end

    test "update_availability/2 with invalid data returns error changeset" do
      availability = availability_fixture()
      assert {:error, %Ecto.Changeset{}} = BookingSystem.update_availability(availability, @invalid_attrs)
      assert availability == BookingSystem.get_availability!(availability.id)
    end

    test "delete_availability/1 deletes the availability" do
      availability = availability_fixture()
      assert {:ok, %Availability{}} = BookingSystem.delete_availability(availability)
      assert_raise Ecto.NoResultsError, fn -> BookingSystem.get_availability!(availability.id) end
    end

    test "change_availability/1 returns a availability changeset" do
      availability = availability_fixture()
      assert %Ecto.Changeset{} = BookingSystem.change_availability(availability)
    end
  end

  
end
