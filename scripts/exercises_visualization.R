# install natverse
# https://natverse.org/install/
# for catmaid, you need the full natverse, not just core
install.packages("natmanager")
natmanager::install('natverse')

library(catmaid)

# establish connection to catmaid server
catmaid_login(
  server = "https://catmaid.jekelylab.ex.ac.uk/",
  authname = "AnonymousUser",
  config = httr::config(ssl_verifypeer = 0, http_version = 1)
)

# read neurons -------------------------

prototroch <- nlapply(
  read.neurons.catmaid(
    "prototroch",
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

MC <- nlapply(
  read.neurons.catmaid(
    "celltype9",
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

cPRC <- nlapply(
  read.neurons.catmaid(
    "ciliary photoreceptor",
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

eyespot_PRC <- nlapply(
  read.neurons.catmaid(
    "eyespot_PRC",
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)

eye <- nlapply(
  read.neurons.catmaid(
    "celltype1",
    pid = 11
  ),
  function(x) smooth_neuron(x, sigma = 6000)
)


# visualize neurons -----------------------------
plot3d(
  prototroch,
  WithConnectors = FALSE, soma = TRUE, lwd = 1,
  add = TRUE, alpha = 0.4,
  col = "grey"
)
plot3d(
  MC,
  WithConnectors = FALSE, soma = TRUE, lwd = 2,
  add = TRUE, alpha = 1,
  col = "red"
)
plot3d(
  cPRC,
  WithConnectors = FALSE, soma = TRUE, lwd = 2,
  add = TRUE, alpha = 0.8,
  col = "cyan"
)
plot3d(
  eyespot_PRC,
  WithConnectors = FALSE, soma = TRUE, lwd = 2,
  add = TRUE, alpha = 0.8,
  col = "blue"
)
plot3d(
  eye,
  WithConnectors = FALSE, soma = TRUE, lwd = 2,
  add = TRUE, alpha = 0.8,
  col = "darkblue"
)

### make a proper figure ----------------------------
yolk <- catmaid_get_volume(
  4,
  rval = "mesh3d",
  invertFaces = T, conn = NULL, pid = 11
)
plot3d(yolk,
       WithConnectors = F, WithNodes = F, soma = F, lwd = 2,
       add = T, alpha = 0.1,
       col = "#E2E2E2"
)
# we define a z clipping plane for the frontal view
par3d(zoom = 0.75)
nview3d("frontal", extramat = rotationMatrix(0.2, 1, 0.1, 0.5))
# z-axis clip
clipplanes3d(0, 0, -1, 85000)
# y-axis clip
clipplanes3d(1, 0, 0.16, 7000)
# x-axis clip
clipplanes3d(0, -1, 0.16, 140000)
par3d(windowRect = c(0, 0, 800, 800)) # resize for frontal view

# save image
dir.create("pictures")
rgl.snapshot("pictures/cells_3D.png")
close3d()
