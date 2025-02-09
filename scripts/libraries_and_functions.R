library(catmaid)

# establish connection to catmaid server
catmaid_login(
  server = "https://catmaid.jekelylab.ex.ac.uk/",
  authname = "AnonymousUser",
  config = httr::config(ssl_verifypeer = 0, http_version = 1)
)

# read volumes ---------------
# These volumes are 3D structures in the animal's body and provide background
# for reconstructed neurons or help create images
{
  outline <- catmaid_get_volume(
    1,
    rval = c("mesh3d", "catmaidmesh", "raw"),
    invertFaces = T, conn = NULL, pid = 11
  )
  yolk <- catmaid_get_volume(
    4,
    rval = c("mesh3d", "catmaidmesh", "raw"),
    invertFaces = T, conn = NULL, pid = 11
  )
  acicula <- nlapply(
    read.neurons.catmaid(
      "^acicula$",
      pid = 11
    ),
    function(x) smooth_neuron(x, sigma = 6000)
  )
  # these four dots are the most extreme points of the volume, adding them to the 3d view solves the problem with automatic zooming and movement of the field shown
  bounding_dots <- nlapply(
    read.neurons.catmaid(
      "^bounding_dots$",
      pid = 11
    ),
    function(x) smooth_neuron(x, sigma = 6000)
  )
  scalebar_50um_anterior <- read.neurons.catmaid("scalebar_50um_anterior", pid = 11)
  scalebar_50um_ventral <- read.neurons.catmaid("scalebar_50um_ventral", pid = 11)
}

plot_background_anterior <- function(x) {
  nopen3d() # opens a pannable 3d window
  plot3d(bounding_dots,
         WithConnectors = F, WithNodes = F, soma = F, lwd = 1,
         add = T, alpha = 1,
         col = "white"
  )
  plot3d(yolk,
         WithConnectors = F, WithNodes = F, soma = F, lwd = 2,
         add = T, alpha = 0.1,
         col = "#E2E2E2"
  )
  # we define a z clipping plane for the frontal view
  par3d(zoom = 0.52)
  nview3d("frontal", extramat = rotationMatrix(0.2, 1, 0.1, 0.5))
  # z-axis clip
  clipplanes3d(0, 0, -1, 65000)
  # y-axis clip
  clipplanes3d(1, 0, 0.16, 7000)
  # x-axis clip
  clipplanes3d(0, -1, 0.16, 130000)
  par3d(windowRect = c(0, 0, 800, 800)) # resize for frontal view
}


# plotting function for ventral view with yolk and acicula
plot_background_ventral <- function(x) {
  nopen3d() # opens a pannable 3d window
  par3d(windowRect = c(20, 30, 600, 800)) # to define the size of the rgl window
  nview3d("ventral", extramat = (rotationMatrix(0.35, 1, 0, 0) %*% rotationMatrix(0.05, 0, 0, 1)))
  par3d(zoom = 0.53)
  plot3d(bounding_dots,
         WithConnectors = F, WithNodes = F, soma = F, lwd = 1,
         add = T, alpha = 1,
         col = "white"
  )
  plot3d(yolk,
         WithConnectors = F, WithNodes = F, soma = F, lwd = 2,
         add = T, alpha = 0.05,
         col = "#E2E2E2"
  )
  plot3d(acicula,
         WithConnectors = F, WithNodes = F, soma = T, lwd = 2,
         add = T, alpha = 1,
         col = "grey70"
  )
  par3d(zoom = 0.48)
}