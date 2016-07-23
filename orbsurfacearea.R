ro <- 0.5  #orb radius
d <- sqrt(2) * ro  # orthogonal distance of orb centroid from axis of rotation
rw <- 0.7444 # radius of water surface to axis of rotation, decision variable
n <- 100000  # sample size

# generate uniform random cartesian points on the cylinder swept by water surface
# let x be the direction of the cylindrical axis

# generate uniform cylindrical coordinates for fixed radius rw
x <- runif(n) - ro  # uniform x coorditates
theta <- 2*pi*runif(n) - pi  # uniform azmuthal angles -pi to pi

# convert cylindrical to cartesian coordinates
y <- rw*cos(theta)
z <- rw*sin(theta)
