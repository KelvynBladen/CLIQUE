
plot_clique <- function(clique, data, var_vec) {

}

library(palmerpenguins)

penguins = palmerpenguins::penguins |> filter(!is.na(bill_length_mm)) |>
  select(!sex)

v <- clique(formula = factor(Species) ~ ., data = iris,
            method = "rf", cores = 2)
v$local_imp

temp = v$local_imp
colnames(temp) = paste0(colnames(temp), "_imp")
df = data.frame(iris, temp)
df

# probably
ggplot(df, aes(x = Petal.Length, y = Petal.Length_imp)) +
  geom_point()

ggplot(df, aes(x = Petal.Length, y = Petal.Length_imp)) +
  geom_point()

# probably
ggplot(df, aes(x = Petal.Length, y = Petal.Length_imp, colour = Species)) +
  geom_point()

# probably
ggplot(df, aes(x = Petal.Length, y = Petal.Length_imp, colour = Petal.Width_imp)) +
  geom_point()

# probably
ggplot(df, aes(x = Petal.Length, y = Petal.Length_imp, colour = Petal.Width)) +
  geom_point()

# overkill
# GGally::ggpairs(temp, 3:4, mapping = aes(colour = iris$Species))

t1 = temp[51, ] |> pivot_longer(cols = everything()) |>
  arrange(desc(value))

t1$name <- factor(t1$name, levels = rev(t1$name))

# yes!
ggplot(t1, aes(x = value, y = name)) + geom_point()

temp$Species = iris$Species

ta = temp |> group_by(Species) |> summarise(across(where(is.numeric), \(x) mean(x))) |>
  pivot_longer(cols = 2:5) |> arrange(desc(value))
ta$name <- factor(ta$name, levels = rev(unique(ta$name)))

# yes!
ggplot(ta, aes(x = value, y = name)) + geom_point() +
  ggh4x::facet_wrap2(~Species, dir = "v", strip.position = "right")

# probably
ggplot(temp, aes(x = Species, y = Petal.Width_imp)) +
  geom_boxplot()


v <- clique(formula = factor(species) ~ ., data = penguins,
            method = "rf", cores = 2, parallel = F)
v$local_imp

temp = v$local_imp
colnames(temp) = paste0(colnames(temp), "_imp")
df = data.frame(penguins, temp)
df

# probably
ggplot(df, aes(x = bill_length_mm, y = bill_length_mm_imp)) +
  geom_point()

# probably
ggplot(df, aes(x = bill_length_mm, y = bill_length_mm_imp, colour = species)) +
  geom_point()

# probably
ggplot(df, aes(x = bill_length_mm, y = bill_length_mm_imp, colour = island)) +
  geom_point()

# overkill
# GGally::ggpairs(temp, 3:4, mapping = aes(colour = iris$Species))

################################################################################
t1 = temp[329, ] |> pivot_longer(cols = everything()) |>
  arrange(desc(value))

t1$name <- factor(t1$name, levels = rev(t1$name))

# yes!
ggplot(t1, aes(x = value, y = name)) + geom_point()

t1 = temp[339, ] |> pivot_longer(cols = everything()) |>
  arrange(desc(value))

t1$name <- factor(t1$name, levels = rev(t1$name))

# yes!
ggplot(t1, aes(x = value, y = name)) + geom_point()

################################################################################

temp$species = penguins$species

ta = temp |> group_by(species) |> summarise(across(where(is.numeric), \(x) mean(x))) |>
  pivot_longer(cols = 2:7) |> arrange(desc(value))
ta$name <- factor(ta$name, levels = rev(unique(ta$name)))

# yes!
ggplot(ta, aes(x = value, y = name)) + geom_point() +
  ggh4x::facet_wrap2(~species, dir = "v", strip.position = "right")

# probably
ggplot(temp, aes(x = species, y = bill_length_mm_imp)) +
  geom_boxplot()

GGally::ggpairs(penguins, c(1, 3:6), mapping = aes(color = species))
GGally::ggpairs(penguins, c(2:6), mapping = aes(color = island))


# heatmap for mnist


