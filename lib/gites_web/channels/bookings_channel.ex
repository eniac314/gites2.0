defmodule GitesWeb.BookingsChannel do 
  use GitesWeb, :channel 

  def join("bookings:locked_days", %{"uuid" => uuid}, socket) do 
  	{:ok, assign(socket, :uuid, uuid)}
  end

  def handle_in("days_locked", payload, socket) do 
    send(self(), {:after_join, "days_locked"})
    {:noreply, socket} 
  end 
  
  def handle_info({:after_join, "days_locked"}, socket) do 
    push(socket, "presence_state", Presence.list(socket))

    {:ok, _} = 
    # track broadcast a diff message to all clients of this channel
      Presence.track(socket, socket.assigns.current_player.username, %{
      	joined_at: inspect(System.system_time(:seconds)),
      	uuid: socket.assigns.uuid
      })

    # IO.inspect(socket)
    {:noreply, socket}
  end

end 