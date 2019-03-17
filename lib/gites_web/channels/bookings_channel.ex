defmodule GitesWeb.BookingsChannel do 
  use GitesWeb, :channel 
  alias GitesWeb.Presence
  alias Gites.LockedAvailabilitiesServer

  def join("bookings:locked_days", %{"uuid" => uuid}, socket) do 
  	send(self(), {:after_join, "days_locked"})
  	{:ok, assign(socket, :uuid, uuid)}
  end

  def handle_in("days_locked", %{"cIn" => cIn, "cOut" => cOut}, socket) do 
    
    LockedAvailabilitiesServer.new_entry({socket.assigns.uuid, cIn, cOut})

    broadcast(socket, "broadcast_locked_days", %{
        uuid: socket.assigns.uuid,
        cIn: cIn, 
        cOut: cOut
    })

    {:noreply, socket} 
  end 
  
  def handle_info({:after_join, "days_locked"}, socket) do 
    
    push(socket, "presence_state", Presence.list(socket))

    push(socket, "broadcast_initial_locked_days", %{payload: LockedAvailabilitiesServer.list_locked})

    {:ok, _} = 
    # track broadcast a diff message to all clients of this channel
      Presence.track(socket, socket.assigns.uuid, %{
      	joined_at: inspect(System.system_time(:second)),
      	uuid: socket.assigns.uuid
      })

    # IO.inspect(socket)
    {:noreply, socket}
  end

end 