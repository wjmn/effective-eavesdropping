# Remove previous build
rm -rf build

# Create a new build directory
mkdir build

# Copy all public assets to build
cp public/* build/

# Rename index.html to iframe.html
mv build/index.html build/iframe.html

# Copy all css from src directory to build
cp src/*.css build/

# Compile application to build/main.js: debug build
# elm-app make src/Main.elm --debug --output build/main.js

# Compile application to build/main.js: optimized build
elm-app make src/Main.elm --optimize --output build/main.js