defmodule Gites.BookingSystem.Availability do
  use Ecto.Schema
  import Ecto.Changeset

  schema "availabilities" do
    field(:availability, :string)
    field(:date, :integer)
    field(:bookingId, :id, default: nil)

    timestamps()
  end

  @doc false
  def changeset(availability, attrs) do
    availability
    |> cast(attrs, [:date, :availability, :bookingId, :id])
    |> validate_required([:date, :availability])
    |> unique_constraint(:date)
  end
end
