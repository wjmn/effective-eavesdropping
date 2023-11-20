# Elm Game Template for the Dr Ecco Heuristic Problem Solving Website

![thumbnail](public/thumbnail.png)

This is a blank game template implementing a simple counter to help you create your game for the Heuristic Problem Solving final project. You might want to also refer to [an example of a completed game (Gomoku) using this template](https://github.com/wjmn/gomoku). 

# Developing

Make sure you have [Elm](https://elm-lang.org/) and [`create-elm-app`](https://github.com/halfzebra/create-elm-app) installed. 

First, clone this repo:

```
git clone https://github.com/wjmn/drecco-game-template
```

To run the development server inside the repository:

```
elm-app start
```

To build the application, run the build script from inside the repository (make sure you have execution permissions):

```
./build.sh
```

# Steps You Need to Complete

1. Go to `public/index.md` and set `title` to your game name and `team` to your team name in the front-matter of the file.
2. Fill in `public/index.md` with your game rules and any other information you want shown on the description page for your game. 
3. Replace `public/thumbnail.png` with a thumbnail image for your game (a small image that will be shown on the home page at about 150px width). You might want to come back to do this after you've completed your game so you can get a screenshot of a representative part of the game. 
3. Code your game in the `src` directory: see the Directory structure below for a brief explanation of each file. There are comments interspersed throughout the code to help guide you through this. I would suggest:
    1. First make sure you have a good mental model of exactly how your game will work. This is absolutely the most important step! If you haven't worked out exactly what all the rules are, do this before you start coding because it will be a headache to try and fix this later. 
    2. Define any really basic utility types and functions you'll use often in `src/Common.elm` (e.g. points, players). 
    2. Define your game settings in `src/Settings.elm`. The comments in this file will guide you through the process. 
    3. Define your game model and logic in `src/Game.elm`. Don't worry about how it will look until you've decided how it will actually work. 
    4. Define your game interface and update logic in the view and update functions of `src/Game.elm`.
    5. Make it look pretty by adding and editing the CSS in `src/main.css`. 

# Directory Structure

Within the root directory of this project:
1. `elm.json` contains information used by Elm (e.g. dependencies). Don't modify this file directly. 
2. `build.sh` is the script used to build a production build ready for deployment to the Dr Ecco website. This replaces the build folder with a new build.  
3. The `src` directory contains all the actual game code. 
4. The `public` directory contains assets that are copied directly to the final production build. 
5. The `tests` directory contains tests for the Elm code.
6. The `elm-stuff` directory (ignored by Git) contains Elm dependencies. Don't modify this folder directly. 
7. The `build` directory (ignored by Git) contains the production build produced by `build.sh`. **Don't try to make changes to files in this folder** as they will just be deleted when `build.sh` is rerun. Leave any changes to files in this folder up to `build.sh`.

Within the `src` folder, there are five Elm files in this project:
1. `src/Main.elm` is the entrypoint that handles delegating to the Settings and Game screen views. 
2. `src/Common.elm` contains basic utility types and functions shared by both `Settings.elm` and `Game.elm`.
3. `src/Settings.elm` contains the model, view and update for the Settings screen. 
4. `src/Game.elm` contains the model, view and update for the Gameplay screen (including all the game logic). 
5. `src/index.js` contains the JavaScript entrypoint for the live development server. This file is not used for the production build. If you modify this file, you **also** need to make sure that you make the same changes to `public/index.html` so that these changes will be reflected in the production build. 
6. `src/main.css` contains all the CSS styling. I recommend just keeping all your CSS styles in this one file and modify it as necessary. **If you add another CSS file, you need to add references to it in both `src/index.js` (for the live development server) and `public/index.html` (for the production build).**

Within the `public` folder, there are three essential static assets:
1. `public/index.html` contains the HTML entrypoint that is used by both the live development server and the production build. 
2. `public/index.md` is a Markdown file that gets converted to the information page for your game. The TOML frontmatter of this file must contain a `title` variable (your game name) as well as an `extra` section with a `team` variable (your team name) and a `thumbnail` variable (the name of the thumbnail image). 
3. `public/thumbnail.png` is a square image used as a thumbnail for your game. Your thumbnail image will be scaled to about 150px wide on the website, so don't make it too detailed. 

You are welcome to add other assets you need to access from your game (e.g. image files) into the `public` folder. They will be copied to the build root directory by the build script. 

# Deployment

If you're just developing a game, don't worry about this section. 

*Note for deployers to the Dr Ecco website*: for deployment, copy the `build` directory into the Dr Ecco website games folder under the relevant year, rename the `build` folder to the game name, then run `zola build` to regenerate the Dr Ecco website.

# If You Don't Use This Template

You are welcome to use JavaScript/TypeScript/any other language which compiles to JavaScript instead of Elm, but you will need to make sure your build follows a few conventions. At a minimum, your final game build will need to be a directory which contains:

1. `index.md`: A Markdown file with TOML frontmatter containing a `title` variable (your game name), and an `extra` section with a `team` variable (your team name) and a `thumbnail` variable (the relative path of a thumbnail image of your game). 
2. `iframe.html`: An HTML file which, when opened, loads your game and everything it needs. If you need to set Settings for your game, you must provide a user interface to set them within this page as well. It must be suitable for serving as a single static HTML file as the entrypoint for your game.
3. `thumbnail.png`: A thumbnail image for your game (which gets shown on the homepage at a width of about 150px). 

You will probably need other assets for your game, which are completely fine - just as long as you have the three files above. 