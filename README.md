# Draftbit Coding Challenge: Prism

The goal of this coding challenge is two part:
- Demonstrate your skillset and how you think about solving problems
- Get a glimpse at the types of problems we're solving at Draftbit

## The Challenge

You'll be recreating Draftbit's `Prism` component for setting margin & padding, and a backend for storing and retrieving the data. The way this works is when you select
a component from the [Layer Tree](./layertree.png "Layer Tree"),  we fetch all the possible props and render them inside the Properties Panel.

This is just one section of the properties panel. Other sections include Flex, Size, Background colors, etc.

![Prism Component](./prism.png "Prism Component")

The prism component lives in a pane called "Margins & Padding". This pane is collapsible.

## Front end

There's a stubbed out `PropertiesPanel` [Rescript](https://rescript-lang.org) component that we import in App.tsx. Please continue the implementation **using ReScript**.

There are 3 high-level states you need to account for:
- default: there is no previous value, the input is pristine.
- changed: a value has been changed from the default value. There is a yellow line shown.
- focused: the input is focused and has a value. Values should include a number and a metric, ie: `100%`, `20pt`, etc.

## Back end

A basic server skeleton has been implemented for you, consisting of the following pieces:

- A `create-tables.sql` file with a dummy table definition in it. You may define your tables in this file. Alternatively you can replace this with an ORM or whatever approach you wish to bootstrap your database.
- A `Dockerfile` which defines a docker container running `Postgres`, seeded with the `create-tables.sql` file. If you run into trouble running docker, you can use another method to run the database such as `Postgres.app` on your computer -- just make sure you modify the scripts in `package.json` and the `.env` accordingly, and document any special steps needed for an engineer to be able to test your server.
- A web server, using `express.js`, in `server.ts`. The server waits for the database to be accessible, connects to it, and implements a dummy JSON API. It's intended purely as an example to get you started -- feel free to change as much as you wish; for example you can use a different server framework, use graphql or protobuf, or whatever you'd like, so long as you write your code in **TypeScript**.
- A `.env` file defining environment variables that will be configured for the web server and some of the scripts in the `package.json`. You may change or add values as necessary.

## Hints
- Some folks can do this in just a few hours, but take as much time as you need. Speed is less important than the finished product.
- Ask as many questions as you need to be successful.
- We're more concerned about the functionality than the appearance: getting it working > getting it pixel perfect.
- **Show your work**. Just like in school, "why" you did something is more valuable than the final answer!

## Project Setup

We've created the base environment for you. It includes Rescript by default, but you're more than welcome to write plain JavaScript. Pick the
experience you're most comfortable with (just know you'll end up learning Reason one way or another here =P).

## Available Scripts

In the project directory, you can run:

### `yarn frontend:start`

Runs the app in the development mode.<br />
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.<br />
You will also see any lint errors in the console.

### `yarn re:start`

Starts the Rescript compiler in watch mode.

### `yarn test`

Launches the test runner in the interactive watch mode.<br />
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

### `yarn re:build`

Build your Rescript files. You must do this before running `yarn build`.

### `yarn build`

Builds the app for production to the `build` folder.<br />
It correctly bundles React in production mode and optimizes the build for the best performance.

### `yarn backend:start`

Runs the backend web server and database together. The same as running `yarn backend:server` and `yarn backend:db` concurrently.

### `yarn backend:server`

Runs the backend web server in `server.ts`. Restarts the server on file changes.

### `yarn backend:build-docker`

Builds the docker container for the backend web server.

### `yarn backend:psql`

Launches a `psql` shell at your local postgres database.

### `yarn backend:db`

Builds the postgres docker image and starts a container running it.

## Learn More

- [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started)
- [React documentation](https://reactjs.org/)
- [Rescript documentation](https://rescript-lang.org)
- [Rescript/React documentation](https://rescript-lang.org/docs/react/latest/introduction)
- [Gentype documentation](https://github.com/cristianoc/genType)
