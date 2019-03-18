defmodule GitesWeb.AvailabilityView do
  use GitesWeb, :view
  alias GitesWeb.AvailabilityView

  def render("index.json", %{availabilities: availabilities}) do
    %{data: render_many(availabilities, AvailabilityView, "availability.json")}
  end

  def render("show.json", %{availability: availability}) do
    %{data: render_one(availability, AvailabilityView, "availability.json")}
  end

  def render("availability.json", %{availability: availability}) do
    %{date: availability.date,
      availability: availability.availability}
  end
end
