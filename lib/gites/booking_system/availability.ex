defmodule Gites.BookingSystem.Availability do
  use Ecto.Schema
  import Ecto.Changeset


  schema "availabilities" do
    field :availability, :string
    field :date, :integer
    field :bookingId, :id

    timestamps()
  end

  @doc false
  def changeset(availability, attrs) do
    availability
    |> cast(attrs, [:date, :availability])
    |> validate_required([:date, :availability])
    |> unique_constraint(:date)
  end
end
