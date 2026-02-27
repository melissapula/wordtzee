# 🎲 Wordtzee

A word-building dice game — Roll, Keep, Spell!

Wordtzee combines the strategic dice-keeping of Yahtzee with word-building mechanics. Roll 7 letter dice, keep the ones you want, reorder them to spell words, and fill 13 scoring categories.

## Features

- **3D animated dice** with cup scoop-and-scatter physics
- **Drag-and-drop** dice management with ghost positioning
- **Offline dictionary** with suffix-stripping validation
- **1-4 players** + optional computer opponent
- **3 difficulty levels** (Easy / Medium / Hard)
- **Progressive Web App** — installable on any device
- **Works offline** after first load

## Quick Start

```bash
npm install
npm run dev
```

Open [http://localhost:5173](http://localhost:5173)

## Deploy to Netlify

### Option A: Connect GitHub repo
1. Push to GitHub
2. Go to [app.netlify.com](https://app.netlify.com)
3. "Add new site" → "Import an existing project"
4. Select your GitHub repo
5. Build settings are auto-detected from `netlify.toml`
6. Click "Deploy"

### Option B: CLI deploy
```bash
npm install -g netlify-cli
netlify login
netlify init         # first time
netlify deploy --prod
```

### Option C: Drag & drop
```bash
npm run build
```
Then drag the `dist/` folder onto [app.netlify.com/drop](https://app.netlify.com/drop)

## Beta Testing

Share your Netlify URL (e.g. `wordtzee.netlify.app`) with testers. On mobile, they can tap the browser's "Add to Home Screen" to install it as an app.

## Tech Stack

- **React 19** + Vite
- **vite-plugin-pwa** (Workbox service worker)
- **CSS animations** (3D transforms, keyframes)
- **Netlify** hosting

## Project Structure

```
wordtzee/
├── public/
│   ├── favicon.svg
│   ├── icon-192.png
│   ├── icon-512.png
│   └── apple-touch-icon.png
├── src/
│   ├── main.jsx          # Entry point
│   ├── index.css          # Global reset & base styles
│   ├── App.jsx            # Root component
│   └── Wordtzee.jsx       # Game (all logic + UI)
├── index.html             # PWA meta tags, font preloads
├── vite.config.js         # Vite + PWA plugin config
├── netlify.toml           # Build & redirect rules
└── package.json
```
# wordtzee
