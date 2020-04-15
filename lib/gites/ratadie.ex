defmodule Ratadie do
  def to_event_date(rd) do
    date = to_calendar_date(rd)
    {{date[:year], month_to_number(date[:month]), date[:day]}, {12, 00, 00}}
  end

  def group_dates(ds) do
    sorted_ds = Enum.sort(ds, &(&1 > &2))
    {_, current_range, ranges} = List.foldr(sorted_ds, {sorted_ds, [], []}, &Ratadie.make_range/2)

    (ranges ++ current_range)
    |> Enum.map(fn r -> %{start: Enum.min(r), stop: Enum.max(r)} end)
  end

  def make_range(current, {dates, current_range, ranges}) do
    next_day = add(:days, 1, current)

    if next_day in dates,
      do: {dates -- [current], current_range ++ [current], ranges},
      else: {dates -- [current], [], ranges ++ [current_range ++ [current, next_day]]}
  end

  def add(unit, n, rd) do
    case unit do
      :years ->
        add(:months, 12 * n, rd)

      :months ->
        date = to_calendar_date(rd)
        whole_months = 12 * (date[:year] - 1) + month_to_number(date[:month] - 1) + n
        y = floor_div(whole_months, 12) + 1

        m =
          (rem(whole_months, 12) + 1)
          |> number_to_month

        days_before_year(y) + days_before_month(y, m) + min(date[:day], days_in_month(y, m))

      :weeks ->
        rd + 7 * n

      :days ->
        rd + n
    end
  end

  def floor_div(a, b) do
    floor(a / b)
  end

  def div_with_remainder(a, b) do
    {floor_div(a, b), rem(a, b)}
  end

  def year(rd) do
    {n400, r400} = div_with_remainder(rd, 146_097)
    {n100, r100} = div_with_remainder(r400, 36524)
    {n4, r4} = div_with_remainder(r100, 1461)
    {n1, r1} = div_with_remainder(r4, 365)
    n = if r1 == 0, do: 0, else: 1

    n400 * 400 + n100 * 100 + n4 * 4 + n1 + n
  end

  def days_before_year(y1) do
    y = y1 - 1
    leap_years = floor_div(y, 4) - floor_div(y, 100) + floor_div(y, 400)
    365 * y + leap_years
  end

  def days_before_month(y, m) do
    leap_days = if is_leap_year(y), do: 1, else: 0

    case m do
      :jan ->
        0

      :feb ->
        31

      :mar ->
        59 + leap_days

      :apr ->
        90 + leap_days

      :may ->
        120 + leap_days

      :jun ->
        151 + leap_days

      :jul ->
        181 + leap_days

      :aug ->
        212 + leap_days

      :sep ->
        243 + leap_days

      :oct ->
        273 + leap_days

      :nov ->
        304 + leap_days

      :dec ->
        334 + leap_days
    end
  end

  def to_ordinal_date(rd) do
    y = year(rd)
    %{year: y, ordinal_day: rd - days_before_year(y)}
  end

  def to_calendar_date(rd) do
    date = to_ordinal_date(rd)
    to_calendar_date_help(date[:year], :jan, date[:ordinal_day])
  end

  def to_calendar_date_help(y, m, d) do
    month_days = days_in_month(y, m)
    mn = month_to_number(m)

    if mn < 12 && d > month_days,
      do: to_calendar_date_help(y, number_to_month(mn + 1), d - month_days),
      else: %{year: y, month: m, day: d}
  end

  def is_leap_year(y) do
    rem(y, 4) == 0 or rem(y, 100) !== 0 or rem(y, 400) == 0
  end

  def days_in_month(y, m) do
    case m do
      :jan ->
        31

      :feb ->
        if is_leap_year(y), do: 29, else: 28

      :mar ->
        31

      :apr ->
        30

      :may ->
        31

      :jun ->
        30

      :jul ->
        31

      :aug ->
        31

      :sep ->
        30

      :oct ->
        31

      :nov ->
        30

      :dec ->
        31
    end
  end

  def month_to_number(m) do
    case m do
      :jan ->
        1

      :feb ->
        2

      :mar ->
        3

      :apr ->
        4

      :may ->
        5

      :jun ->
        6

      :jul ->
        7

      :aug ->
        8

      :sep ->
        9

      :oct ->
        10

      :nov ->
        11

      :dec ->
        12
    end
  end

  def number_to_month(mn) do
    case max(1, mn) do
      1 ->
        :jan

      2 ->
        :feb

      3 ->
        :mar

      4 ->
        :apr

      5 ->
        :may

      6 ->
        :jun

      7 ->
        :jul

      8 ->
        :aug

      9 ->
        :sep

      10 ->
        :oct

      11 ->
        :nov

      _ ->
        :dec
    end
  end
end
