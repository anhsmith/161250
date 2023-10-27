library(gganimate)
library(gifski)

v <- c(8.2,9,9.3,9.5,9.6,10,10.8)

xkd <- expand_grid(x = seq(6,13,length=100),
                   vals = v) |>
  mutate(y=dnorm(x,mean=vals,sd=.5)/7) |>
  arrange(vals) |>
  group_by(x) |>
  mutate(ycum = cumsum(y)) |>
  ungroup() |>
  mutate(y1 = y) |>
  mutate(y2 = if_else(vals == v[2], ycum,
                      if_else(vals < v[2], NA, y))) |>
  mutate(y3 = if_else(vals == v[3], ycum,
                      if_else(vals < v[3], NA, y))) |>
  mutate(y4 = if_else(vals == v[4], ycum,
                      if_else(vals < v[4], NA, y))) |>
  mutate(y5 = if_else(vals == v[5], ycum,
                      if_else(vals < v[5], NA, y))) |>
  mutate(y6 = if_else(vals == v[6], ycum,
                      if_else(vals < v[6], NA, y))) |>
  mutate(y7 = if_else(vals == v[7], ycum, NA))

xkdtotal <- xkd |>
  summarise(ytotal = sum(y), .by = x)

ggplot() + 
  geom_area(
    data = xkdtotal,
    mapping = aes(x=x, y = ytotal),
    alpha=.2, col=1, position="identity") + 
  geom_area(
    data = xkd,
    mapping = aes(x=x, y=y, 
                  group=factor(vals), 
                  fill=factor(vals)),
    alpha=.2, col=1, 
    position="identity") + 
  theme(legend.position = "none") + 
  geom_point(mapping=aes(x=v, y=0), size=3, inherit.aes = F) +
  ylab("Density")

ggsave(filename = "img/kde.png", width = 5, height = 4)

xkd |>
  pivot_longer(cols = y1:y7) |>
  ggplot() +
  aes(x=x, y=value, group=factor(vals)) +
  
  geom_area(mapping=aes(fill=factor(vals)),
            alpha=.2, col=1, position="identity") +
  
  geom_area(position="identity", alpha=.2, col=1) +
  geom_point(mapping=aes(x=vals, y=0), size=3, inherit.aes = F) +
  theme(legend.position="none") +
  ylab("Density") + xlab(expression(italic(x))) +
  theme(axis.title.x = element_text(family = "serif")) +
  ylim(0,.5) + xlim(6,13) +
  transition_states(name,
                    transition_length = 4,
                    state_length = 5, wrap=FALSE
  ) +
  NULL
