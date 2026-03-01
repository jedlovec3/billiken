import os
import re
import csv
from flask import Flask, jsonify, render_template, request

app = Flask(__name__)

SCENARIOS_DIR   = os.path.join(os.path.dirname(__file__), "data", "scenarios")
TRADE_DEFS_DIR  = os.path.join(os.path.dirname(__file__), "scenarios")


def get_scenarios():
    scenarios = []
    try:
        for name in sorted(os.listdir(SCENARIOS_DIR)):
            if name == "_baseline":
                continue
            latest = os.path.join(SCENARIOS_DIR, name, "latest.txt")
            if os.path.isdir(os.path.join(SCENARIOS_DIR, name)) and os.path.exists(latest):
                display = name.replace("_", " ").title()
                has_def = os.path.exists(os.path.join(TRADE_DEFS_DIR, f"{name}.csv"))
                scenarios.append({"value": name, "label": display, "has_definition": has_def})
    except FileNotFoundError:
        pass
    return scenarios


def write_scenario_csv(path, rows):
    with open(path, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["player", "from_team", "to_team", "round", "pick", "ForceKeeper", "DropPenalty"])
        for row in rows:
            row_type = row.get("type")
            if row_type == "player":
                player       = row.get("player", "").strip()
                from_team    = row.get("from_team", "").strip()
                to_team      = row.get("to_team", "").strip()
                force_keeper = row.get("force_keeper", "")
                drop_penalty = row.get("drop_penalty", "")
                if not player or not from_team:
                    raise ValueError("Player rows require player name and from_team")
                writer.writerow([player, from_team, to_team, "", "", force_keeper, drop_penalty])
            elif row_type == "pick":
                from_team = row.get("from_team", "").strip()
                to_team   = row.get("to_team", "").strip()
                rnd       = row.get("round", "")
                pick      = row.get("pick", "")
                if not from_team or not to_team or not rnd or not pick:
                    raise ValueError("Pick rows require from_team, to_team, round, and pick")
                writer.writerow(["", from_team, to_team, rnd, pick, "", ""])
            else:
                raise ValueError(f"Unknown row type: {row_type}")


def load_delta(scenario_name):
    latest_path = os.path.join(SCENARIOS_DIR, scenario_name, "latest.txt")
    with open(latest_path) as f:
        run_folder = os.path.basename(f.readline().strip())
    csv_path = os.path.join(SCENARIOS_DIR, scenario_name, run_folder, "delta_summary.csv")

    COLS = {
        "team": "Team",
        "baseline_avg_pts": "Baseline Pts",
        "scenario_avg_pts": "Scenario Pts",
        "delta_avg_pts": "\u0394 Pts",
        "delta_avg_rank": "\u0394 Rank",
        "delta_wins": "\u0394 Wins",
        "delta_top_3": "\u0394 Top 3",
        "delta_avg_hit_pts": "\u0394 Hit Pts",
        "delta_avg_pit_pts": "\u0394 Pitch Pts",
    }

    rows = []
    with open(csv_path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            record = {}
            for src, dst in COLS.items():
                val = row.get(src, "")
                if src != "team":
                    try:
                        val = round(float(val), 1)
                    except (ValueError, TypeError):
                        pass
                record[dst] = val
            rows.append(record)

    rows.sort(key=lambda r: r.get("\u0394 Pts", 0), reverse=True)
    return rows


def sanitize_name(name):
    name = name.strip().lower()
    name = re.sub(r"[^a-z0-9]+", "_", name)
    name = name.strip("_")
    return name


@app.route("/")
def index():
    scenarios = get_scenarios()
    return render_template("index.html", scenarios=scenarios)


@app.route("/api/scenario/<name>")
def scenario_data(name):
    safe = os.path.basename(name)
    if safe != name or not os.path.isdir(os.path.join(SCENARIOS_DIR, safe)):
        return jsonify({"error": "Invalid scenario"}), 400
    try:
        rows = load_delta(safe)
        return jsonify(rows)
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route("/api/scenarios", methods=["POST"])
def save_scenario():
    data = request.get_json(silent=True)
    if not data:
        return jsonify({"error": "Invalid JSON"}), 400

    raw_name = data.get("name", "").strip()
    if not raw_name:
        return jsonify({"error": "Scenario name is required"}), 400

    name = sanitize_name(raw_name)
    if not name:
        return jsonify({"error": "Scenario name contains no valid characters"}), 400

    rows = data.get("rows", [])
    if not rows:
        return jsonify({"error": "At least one trade row is required"}), 400

    os.makedirs(TRADE_DEFS_DIR, exist_ok=True)
    dest = os.path.join(TRADE_DEFS_DIR, f"{name}.csv")
    if os.path.exists(dest):
        return jsonify({"error": f"Scenario '{name}' already exists"}), 409

    try:
        write_scenario_csv(dest, rows)
    except ValueError as e:
        return jsonify({"error": str(e)}), 400

    return jsonify({"name": name}), 201


@app.route("/api/scenarios/<name>/definition")
def get_scenario_definition(name):
    safe = os.path.basename(name)
    if safe != name:
        return jsonify({"error": "Invalid name"}), 400

    path = os.path.join(TRADE_DEFS_DIR, f"{safe}.csv")
    if not os.path.exists(path):
        return jsonify({"error": "No definition file found for this scenario"}), 404

    rows = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            player = row.get("player", "").strip()
            if not player:
                rows.append({
                    "type":      "pick",
                    "from_team": row.get("from_team", "").strip().upper(),
                    "to_team":   row.get("to_team", "").strip().upper(),
                    "round":     row.get("round", "").strip(),
                    "pick":      row.get("pick", "").strip(),
                })
            else:
                rows.append({
                    "type":         "player",
                    "player":       player,
                    "from_team":    row.get("from_team", "").strip().upper(),
                    "to_team":      row.get("to_team", "").strip().upper(),
                    "force_keeper": row.get("ForceKeeper", "").strip(),
                    "drop_penalty": row.get("DropPenalty", "").strip(),
                })

    return jsonify({"name": safe, "rows": rows})


@app.route("/api/scenarios/<name>", methods=["PUT"])
def update_scenario(name):
    safe = os.path.basename(name)
    if safe != name:
        return jsonify({"error": "Invalid name"}), 400

    path = os.path.join(TRADE_DEFS_DIR, f"{safe}.csv")
    if not os.path.exists(path):
        return jsonify({"error": "Scenario not found"}), 404

    data = request.get_json(silent=True)
    if not data:
        return jsonify({"error": "Invalid JSON"}), 400

    rows = data.get("rows", [])
    if not rows:
        return jsonify({"error": "At least one trade row is required"}), 400

    try:
        write_scenario_csv(path, rows)
    except ValueError as e:
        return jsonify({"error": str(e)}), 400

    return jsonify({"name": safe})


if __name__ == "__main__":
    from threading import Thread

    extra_ports = [65535]
    for p in extra_ports:
        t = Thread(target=lambda port=p: app.run(host="0.0.0.0", port=port, debug=False, use_reloader=False), daemon=True)
        t.start()

    app.run(host="0.0.0.0", port=5000, debug=False, use_reloader=False)
