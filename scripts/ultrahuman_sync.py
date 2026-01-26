#!/usr/bin/env python3
"""
Ultrahuman Ring API sync for futon0 vitality tracking.

Pulls daily metrics and writes to JSON for futon0-rhythm.el integration.

Usage:
    python3 ultrahuman_sync.py
    python3 ultrahuman_sync.py --date 2026-01-18
    python3 ultrahuman_sync.py --days 7  # sync last 7 days

Environment:
    ULTRAHUMAN_API_KEY - API key from Ultrahuman developer portal
    ULTRAHUMAN_USER_ID - Your user ID (optional, uses default if authenticated)

Output:
    ~/code/storage/futon0/vitality/ultrahuman/ultrahuman_daily.json  (latest)
    ~/code/storage/futon0/vitality/ultrahuman/YYYY-MM-DD.json        (archive)
"""

import argparse
import json
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

# Optional: requests library for API calls
try:
    import requests
    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False

# Configuration
DEFAULT_OUTPUT_DIR = Path.home() / "code/storage/futon0/vitality/ultrahuman"
ULTRAHUMAN_API_BASE = "https://api.ultrahuman.com/v1"  # Placeholder - verify actual endpoint

# Metrics we care about for rhythm tracking (subset of full API)
RHYTHM_METRICS = [
    "recovery",
    "avg_sleep_hrv",
    "total_sleep_minutes",
    "deep_sleep_pct",
    "sleep_score",
    "hr_drop",
    "active_minutes",
    "steps",
    "night_rhr",
    "sleep_efficiency",
]


def get_api_key():
    """Get API key from environment."""
    key = os.environ.get("ULTRAHUMAN_API_KEY")
    if not key:
        print("Error: ULTRAHUMAN_API_KEY not set", file=sys.stderr)
        print("Get your API key from Ultrahuman developer portal", file=sys.stderr)
        sys.exit(1)
    return key


def fetch_daily_metrics(date: str, api_key: str) -> dict:
    """
    Fetch metrics for a specific date from Ultrahuman API.

    Args:
        date: Date string in YYYY-MM-DD format
        api_key: Ultrahuman API key

    Returns:
        Dict with metrics, or None on error
    """
    if not HAS_REQUESTS:
        print("Error: requests library required. pip install requests", file=sys.stderr)
        return None

    # TODO: Verify actual Ultrahuman API endpoint and auth method
    # This is a placeholder based on typical health API patterns
    headers = {
        "Authorization": f"Bearer {api_key}",
        "Content-Type": "application/json",
    }

    try:
        # Endpoint TBD - this is speculative
        url = f"{ULTRAHUMAN_API_BASE}/metrics/daily"
        params = {"date": date}

        response = requests.get(url, headers=headers, params=params, timeout=30)
        response.raise_for_status()

        data = response.json()
        return extract_rhythm_metrics(data, date)

    except requests.exceptions.RequestException as e:
        print(f"API error for {date}: {e}", file=sys.stderr)
        return None


def extract_rhythm_metrics(raw_data: dict, date: str) -> dict:
    """
    Extract the metrics we care about for rhythm tracking.

    Args:
        raw_data: Full API response
        date: Date string

    Returns:
        Filtered dict with rhythm-relevant metrics
    """
    result = {"date": date, "synced_at": datetime.now().isoformat()}

    for metric in RHYTHM_METRICS:
        if metric in raw_data:
            result[metric] = raw_data[metric]
        # Handle nested structures if API uses them
        elif "metrics" in raw_data and metric in raw_data["metrics"]:
            result[metric] = raw_data["metrics"][metric]
        elif "sleep" in raw_data and metric in raw_data["sleep"]:
            result[metric] = raw_data["sleep"][metric]
        elif "recovery" in raw_data and isinstance(raw_data["recovery"], dict):
            if metric in raw_data["recovery"]:
                result[metric] = raw_data["recovery"][metric]

    return result


def save_metrics(metrics: dict, output_dir: Path, date: str):
    """
    Save metrics to JSON files.

    Creates:
        - ultrahuman_daily.json (latest, for futon0-rhythm.el)
        - YYYY-MM-DD.json (archive)
    """
    output_dir.mkdir(parents=True, exist_ok=True)

    # Archive file
    archive_file = output_dir / f"{date}.json"
    with open(archive_file, "w") as f:
        json.dump(metrics, f, indent=2)
    print(f"Saved: {archive_file}")

    # Latest file (for futon0-rhythm.el)
    latest_file = output_dir / "ultrahuman_daily.json"
    with open(latest_file, "w") as f:
        json.dump(metrics, f, indent=2)
    print(f"Updated: {latest_file}")


def create_mock_metrics(date: str) -> dict:
    """
    Create mock metrics for testing without API access.

    Remove this once real API integration is working.
    """
    import random

    return {
        "date": date,
        "synced_at": datetime.now().isoformat(),
        "recovery": random.randint(55, 85),
        "avg_sleep_hrv": random.randint(35, 60),
        "total_sleep_minutes": random.randint(360, 480),
        "deep_sleep_pct": random.randint(12, 25),
        "sleep_score": random.randint(65, 90),
        "hr_drop": random.randint(15, 30),
        "active_minutes": random.randint(20, 90),
        "steps": random.randint(3000, 12000),
        "night_rhr": random.randint(48, 62),
        "sleep_efficiency": random.randint(80, 95),
        "_mock": True,
    }


def main():
    parser = argparse.ArgumentParser(
        description="Sync Ultrahuman ring metrics for futon0 rhythm tracking"
    )
    parser.add_argument(
        "--date",
        help="Specific date to sync (YYYY-MM-DD), default today",
        default=datetime.now().strftime("%Y-%m-%d"),
    )
    parser.add_argument(
        "--days",
        type=int,
        help="Sync last N days",
        default=1,
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Output directory",
        default=DEFAULT_OUTPUT_DIR,
    )
    parser.add_argument(
        "--mock",
        action="store_true",
        help="Generate mock data (for testing without API)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Print metrics without saving",
    )

    args = parser.parse_args()

    # Determine dates to sync
    if args.days > 1:
        dates = [
            (datetime.now() - timedelta(days=i)).strftime("%Y-%m-%d")
            for i in range(args.days)
        ]
    else:
        dates = [args.date]

    # Get API key (skip if mock mode)
    api_key = None
    if not args.mock:
        api_key = get_api_key()

    # Sync each date
    for date in dates:
        print(f"\nSyncing {date}...")

        if args.mock:
            metrics = create_mock_metrics(date)
        else:
            metrics = fetch_daily_metrics(date, api_key)

        if metrics:
            if args.dry_run:
                print(json.dumps(metrics, indent=2))
            else:
                save_metrics(metrics, args.output_dir, date)
        else:
            print(f"No data for {date}")

    print("\nDone.")


if __name__ == "__main__":
    main()
