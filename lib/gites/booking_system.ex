defmodule Gites.BookingSystem do
  @moduledoc """
  The BookingSystem context.
  """

  import Ecto.Query, warn: false
  alias Gites.Repo

  alias Gites.BookingSystem.Booking

  @doc """
  Returns the list of bookings.

  ## Examples

      iex> list_bookings()
      [%Booking{}, ...]

  """
  def list_bookings do
    Repo.all(Booking)
  end

  @doc """
  Gets a single booking.

  Raises `Ecto.NoResultsError` if the Booking does not exist.

  ## Examples

      iex> get_booking!(123)
      %Booking{}

      iex> get_booking!(456)
      ** (Ecto.NoResultsError)

  """
  def get_booking!(id), do: Repo.get!(Booking, id)

  @doc """
  Creates a booking.

  ## Examples

      iex> create_booking(%{field: value})
      {:ok, %Booking{}}

      iex> create_booking(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_booking(attrs \\ %{}) do
    %Booking{}
    |> Booking.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a booking.

  ## Examples

      iex> update_booking(booking, %{field: new_value})
      {:ok, %Booking{}}

      iex> update_booking(booking, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_booking(%Booking{} = booking, attrs) do
    booking
    |> Booking.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Booking.

  ## Examples

      iex> delete_booking(booking)
      {:ok, %Booking{}}

      iex> delete_booking(booking)
      {:error, %Ecto.Changeset{}}

  """
  def delete_booking(%Booking{} = booking) do
    Repo.delete(booking)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking booking changes.

  ## Examples

      iex> change_booking(booking)
      %Ecto.Changeset{source: %Booking{}}

  """
  def change_booking(%Booking{} = booking) do
    Booking.changeset(booking, %{})
  end

  alias Gites.BookingSystem.Availability

  @doc """
  Returns the list of availabilities.

  ## Examples

      iex> list_availabilities()
      [%Availability{}, ...]

  """
  def list_availabilities do
    Repo.all(Availability)
  end

  @doc """
  Gets a single availability.

  Raises `Ecto.NoResultsError` if the Availability does not exist.

  ## Examples

      iex> get_availability!(123)
      %Availability{}

      iex> get_availability!(456)
      ** (Ecto.NoResultsError)

  """
  def get_availability!(id), do: Repo.get!(Availability, id)

  @doc """
  Creates a availability.

  ## Examples

      iex> create_availability(%{field: value})
      {:ok, %Availability{}}

      iex> create_availability(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def bulk_create_availabilities(id, availabilities) do    
    res = availabilities
          |> Enum.map(fn av -> Map.put(av, "bookingId", id) end)
          |> Enum.map(&Gites.BookingSystem.create_availability/1)
    {:ok, res}
  end 

  def create_availability(attrs \\ %{}) do
    %Availability{}
    |> Availability.changeset(attrs)
    |> Repo.insert(on_conflict: :replace_all, conflict_target: :date)
  end

  @doc """
  Updates a availability.

  ## Examples

      iex> update_availability(availability, %{field: new_value})
      {:ok, %Availability{}}

      iex> update_availability(availability, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_availability(%Availability{} = availability, attrs) do
    availability
    |> Availability.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Availability.

  ## Examples

      iex> delete_availability(availability)
      {:ok, %Availability{}}

      iex> delete_availability(availability)
      {:error, %Ecto.Changeset{}}

  """
  def delete_availability(%Availability{} = availability) do
    Repo.delete(availability)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking availability changes.

  ## Examples

      iex> change_availability(availability)
      %Ecto.Changeset{source: %Availability{}}

  """
  def change_availability(%Availability{} = availability) do
    Availability.changeset(availability, %{})
  end
end
