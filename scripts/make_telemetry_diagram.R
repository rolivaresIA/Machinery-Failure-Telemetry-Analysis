# Draws the telemetry data-flow diagram used in the README (images/telemetry_overview.png).
# Usage (from the repository root): Rscript scripts/make_telemetry_diagram.R
suppressPackageStartupMessages(library(ggplot2))

boxes <- data.frame(
  x = c(1, 3.4, 5.8, 8.2, 10.6),
  label = c("Machine\n(engine, hydraulics,\nGPS, hour meter)",
            "Telematics\ngateway\n(cellular / satellite)",
            "Cloud platform\n(alert engine)",
            "Dealer\nafter-sales analytics",
            "Customer\ncontact & service"),
  fill = c("#dfe8ee", "#dfe8ee", "#dfe8ee", "#2f6f8f", "#dfe8ee"),
  txt  = c("#1c2b33", "#1c2b33", "#1c2b33", "#ffffff", "#1c2b33")
)

arrows <- data.frame(x = boxes$x[-5] + 0.95, xend = boxes$x[-1] - 0.95, y = 1, yend = 1)

p <- ggplot() +
  geom_rect(data = boxes, aes(xmin = x - 0.9, xmax = x + 0.9, ymin = 0.35, ymax = 1.65, fill = fill),
            color = "#5b7584", linewidth = 0.5) +
  geom_text(data = boxes, aes(x = x, y = 1, label = label, color = txt),
            size = 3.4, lineheight = 0.95, fontface = "bold") +
  geom_segment(data = arrows, aes(x = x, xend = xend, y = y, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               color = "#5b7584", linewidth = 0.7) +
  annotate("text", x = 1, y = 0.1, label = "alerts + hours + position", size = 3, color = "#5b7584") +
  annotate("text", x = 5.8, y = 0.1, label = "alert level: INFO / YELLOW / RED", size = 3, color = "#5b7584") +
  annotate("text", x = 9.4, y = 0.1, label = "predict & prioritise  >  proactive action", size = 3,
           color = "#2f6f8f", fontface = "bold") +
  scale_fill_identity() + scale_color_identity() +
  coord_cartesian(xlim = c(-0.2, 11.8), ylim = c(-0.1, 2)) +
  labs(title = "Heavy-equipment telemetry: from the machine to proactive after-sales action") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5,
                                  margin = margin(t = 8, b = 4)),
        plot.background = element_rect(fill = "white", color = NA))

dir.create("images", showWarnings = FALSE)
ggsave("images/telemetry_overview.png", p, width = 11, height = 3.2, dpi = 150)
