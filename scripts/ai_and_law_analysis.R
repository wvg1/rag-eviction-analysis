### analysis for AI and Law manuscript ###
### reproduces Figures 2, 3, and 4 ###

# ---- setup ----------------------------------------------------------------
# set working directory depending on context
setwd("rag-eviction-analysis")

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)

dir.create("figs", showWarnings = FALSE)

# ---- read + clean ---------------------------------------------------------
df <- readRDS("data/final_merged_data.rds") |>
  as_tibble() |>
  # residential cases only (drop the 88 commercial cases)
  filter(!commercial_flag) |>
  # require a valid geocode so each case can be placed in/outside Tacoma
  filter(!is.na(lat), !is.na(lon)) |>
  mutate(
    # Tacoma indicator, robust to stray whitespace/case in the city field
    location = factor(
      if_else(str_to_lower(str_trim(address_city)) == "tacoma",
              "Tacoma", "Outside Tacoma"),
      levels = c("Outside Tacoma", "Tacoma")
    ),
    # time helpers
    file_date  = as.Date(file_date),
    file_month = floor_date(file_date, "month"),
    year       = factor(year, levels = c("2022", "2023", "2024")),
    # readable outcome names (reconciled *_final variables, as in the paper)
    responded        = defendant_appearance,
    hearing          = hearing_held,
    attended         = defendant_hearing_attendance,
    represented      = defendant_rep_merged,
    default_judgment = writ_final & !hearing_held,        # judgment, no hearing
    response_status  = factor(if_else(responded, "Submitted response", "No response"),
                              levels = c("No response", "Submitted response"))
  )

# ---- shared styling -------------------------------------------------------
pal <- c(response = "#2C6E91", hearing = "#E08214", attendance = "#5AAE61",
         tacoma = "#B2182B", elsewhere = "#4D4D4D", filings = "#C9D6DE")

theme_eviction <- theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = rel(1.15)),
    plot.subtitle      = element_text(color = "grey35", margin = margin(b = 10)),
    plot.caption       = element_text(color = "grey50", size = rel(0.8), hjust = 0),
    axis.title         = element_text(color = "grey25"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank(),
    plot.margin        = margin(15, 20, 15, 15)
  )

# ===========================================================================
# Figure 2: Eviction filings and procedural outcomes
# ===========================================================================
monthly <- df |>
  group_by(file_month) |>
  summarise(filings = n(),
            response   = mean(responded),
            hearing    = mean(hearing),
            attendance = mean(attended),
            .groups = "drop")

max_filings <- max(monthly$filings)  # scale factor for the secondary axis

rate_long <- monthly |>
  select(file_month, response, hearing, attendance) |>
  pivot_longer(-file_month, names_to = "metric", values_to = "rate") |>
  mutate(metric = factor(metric,
                         levels = c("response", "hearing", "attendance"),
                         labels = c("Written response filed", "Hearing held",
                                    "Hearing attended")))

fig2 <- ggplot(monthly, aes(file_month)) +
  geom_col(aes(y = filings), fill = pal["filings"], width = 25) +
  geom_line(data = rate_long, aes(y = rate * max_filings, color = metric), linewidth = 0.9) +
  geom_point(data = rate_long, aes(y = rate * max_filings, color = metric), size = 1.1) +
  scale_y_continuous(name = "Eviction filings per month", labels = comma,
                     sec.axis = sec_axis(~ . / max_filings, name = "Share of cases",
                                         labels = percent_format(accuracy = 1))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(0.01)) +
  scale_color_manual(values = unname(pal[c("response", "hearing", "attendance")])) +
  labs(title = "Eviction filings and procedural outcomes",
       subtitle = "Pierce County, WA residential eviction cases, 2022\u20132024", x = NULL,
       caption = "Bars: monthly filing volume. Lines: share of the month's cases reaching each checkpoint.") +
  theme_eviction

ggsave("figs/figure2_filings_procedural.png", fig2, width = 10, height = 6, dpi = 200, bg = "white")

# ===========================================================================
# Figure 3: Default eviction judgments in cases with no hearing
# ===========================================================================
# (a) monthly trend, Tacoma vs. elsewhere
monthly_def <- df |>
  group_by(location, file_month) |>
  summarise(default_rate = mean(default_judgment), .groups = "drop")

fig3_monthly <- ggplot(monthly_def, aes(file_month, default_rate, color = location)) +
  geom_vline(xintercept = as.Date("2023-10-01"), linetype = "dashed", color = "grey60") +
  annotate("text", x = as.Date("2023-10-01"), y = 0.52, label = "Oct 2023",
           hjust = -0.1, size = 3.2, color = "grey45") +
  geom_line(alpha = 0.35, linewidth = 0.5) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1.1) +
  scale_color_manual(values = unname(pal[c("elsewhere", "tacoma")])) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(c(0, 0.05))) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y", expand = expansion(0.01)) +
  labs(title = "Default eviction judgments in cases with no hearing",
       subtitle = "Share of cases with an eviction judgment entered and no hearing held",
       x = NULL, y = "Share of all cases",
       caption = "Thin lines: monthly rate. Thick lines: smoothed trend. Dashed line marks Oct 2023.") +
  theme_eviction

ggsave("figs/figure3_default_judgments_monthly.png", fig3_monthly,
       width = 10, height = 6, dpi = 200, bg = "white")

# (b) compact yearly summary (reproduces the paper's headline numbers)
yearly_def <- df |>
  group_by(year, location) |>
  summarise(default_rate = mean(default_judgment), .groups = "drop")

fig3_yearly <- ggplot(yearly_def, aes(year, default_rate, fill = location)) +
  geom_col(position = position_dodge(0.7), width = 0.65) +
  geom_text(aes(label = percent(default_rate, accuracy = 0.1)),
            position = position_dodge(0.7), vjust = -0.4, size = 3.3, color = "grey20") +
  scale_fill_manual(values = unname(pal[c("elsewhere", "tacoma")])) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(c(0, 0.12))) +
  labs(title = "Default eviction judgments without a hearing, by year",
       subtitle = "Tacoma diverges sharply after its no-default-judgment ordinance takes hold",
       x = NULL, y = "Share of all cases") +
  theme_eviction

ggsave("figs/figure3_default_judgments_yearly.png", fig3_yearly,
       width = 8, height = 5.5, dpi = 200, bg = "white")

# ===========================================================================
# Figure 4: Representation rates by status of written responses
# ===========================================================================
rep_by_year <- df |>
  group_by(year, response_status) |>
  summarise(rep_rate = mean(represented), .groups = "drop")

rep_overall <- df |>
  group_by(response_status) |>
  summarise(rep_rate = mean(represented), .groups = "drop") |>
  mutate(year = "All years")

rep_df <- bind_rows(rep_by_year, rep_overall) |>
  mutate(year = factor(year, levels = c("All years", "2022", "2023", "2024")))

fig4 <- ggplot(rep_df, aes(year, rep_rate, fill = response_status)) +
  geom_col(position = position_dodge(0.72), width = 0.66) +
  geom_text(aes(label = percent(rep_rate, accuracy = 1)),
            position = position_dodge(0.72), vjust = -0.4, size = 3.3, color = "grey20") +
  scale_fill_manual(values = c("No response" = "#BCC4CC",
                               "Submitted response" = unname(pal["response"]))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1), expand = expansion(c(0, 0.04))) +
  labs(title = "Representation rates by status of written responses",
       subtitle = "Tenants who engage early are far more likely to obtain counsel",
       x = NULL, y = "Received legal representation") +
  theme_eviction

ggsave("figs/figure4_representation.png", fig4, width = 8.5, height = 5.5, dpi = 200, bg = "white")

message("Done. Four PNGs written to figs/.")