import "dotenv/config";
import { Client } from "pg";
import { backOff } from "exponential-backoff";
import express from "express";
import waitOn from "wait-on";
import onExit from "signal-exit";
import cors from "cors";

// Add your routes here
const setupApp = (client: Client): express.Application => {
  const app: express.Application = express();

  app.use(cors());

  app.use(express.json());

  app.get("/examples", async (_req, res) => {
    const { rows } = await client.query(`SELECT * FROM example_table`);
    res.json(rows);
  });

  // Get element styles
  app.get("/element/:id/styles", async (req, res) => {
    const { id } = req.params;
    try {
      const { rows } = await client.query(
        `SELECT * FROM element_styles WHERE element_id = $1`,
        [id]
      );
      if (rows.length === 0) {
        res.status(404).json({ error: "Element styles not found" });
      } else {
        res.json(rows[0]);
      }
    } catch (error) {
      res.status(500).json({ error: "Internal server error" });
    }
  });

  // Update element styles
  app.put("/element/:id/styles", async (req, res) => {
    const { id } = req.params;
    const {
      margin_top,
      margin_right,
      margin_bottom,
      margin_left,
      padding_top,
      padding_right,
      padding_bottom,
      padding_left
    } = req.body;

    try {
      const { rows } = await client.query(
        `UPDATE element_styles
         SET margin_top = $1, margin_right = $2, margin_bottom = $3, margin_left = $4,
             padding_top = $5, padding_right = $6, padding_bottom = $7, padding_left = $8
         WHERE element_id = $9
         RETURNING *`,
        [margin_top, margin_right, margin_bottom, margin_left,
         padding_top, padding_right, padding_bottom, padding_left, id]
      );

      if (rows.length === 0) {
        res.status(404).json({ error: "Element styles not found" });
      } else {
        res.json(rows[0]);
      }
    } catch (error) {
      res.status(500).json({ error: "Internal server error" });
    }
  });

  return app;
};

// Waits for the database to start and connects
const connect = async (): Promise<Client> => {
  console.log("Connecting");
  const resource = `tcp:${process.env.PGHOST}:${process.env.PGPORT}`;
  console.log(`Waiting for ${resource}`);
  await waitOn({ resources: [resource] });
  console.log("Initializing client");
  const client = new Client();
  await client.connect();
  console.log("Connected to database");

  // Ensure the client disconnects on exit
  onExit(async () => {
    console.log("onExit: closing client");
    await client.end();
  });

  return client;
};

const main = async () => {
  const client = await connect();
  const app = setupApp(client);
  const port = parseInt(process.env.SERVER_PORT);
  app.listen(port, () => {
    console.log(
      `Draftbit Coding Challenge is running at http://localhost:${port}/`
    );
  });
};

main();
