# For a demo:
# -----------

# To test locally while developing ----------------------------------------

plumber::plumb('plumber.R')$run(host = "0.0.0.0", port = 8000)

# Then open your browser at
# http://localhost:8000/__swagger__/
