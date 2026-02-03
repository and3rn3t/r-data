#!/bin/bash
# Deploy Iowa Cities Dashboard to Hugging Face Spaces
#
# Prerequisites:
# 1. Install Hugging Face CLI: pip install huggingface_hub
# 2. Login: huggingface-cli login (use your HF token)
# 3. Create a new Space at https://huggingface.co/new-space
#    - Select "Docker" as the SDK
#    - Name it something like "iowa-cities-dashboard"

set -e

# Configuration
HF_USERNAME="${HF_USERNAME:-your-username}"
SPACE_NAME="${SPACE_NAME:-iowa-cities-dashboard}"
SPACE_REPO="$HF_USERNAME/$SPACE_NAME"

echo "🚀 Deploying Iowa Cities Dashboard to Hugging Face Spaces"
echo "   Target: https://huggingface.co/spaces/$SPACE_REPO"
echo ""

# Check if huggingface-cli is installed
if ! command -v huggingface-cli &> /dev/null; then
    echo "❌ huggingface-cli not found. Install with: pip install huggingface_hub"
    exit 1
fi

# Create temporary directory for deployment
DEPLOY_DIR=$(mktemp -d)
echo "📁 Preparing deployment in $DEPLOY_DIR"

# Copy necessary files
cp Dockerfile.huggingface "$DEPLOY_DIR/Dockerfile"
cp README.hf.md "$DEPLOY_DIR/README.md"
cp app.R "$DEPLOY_DIR/"
cp config.yml "$DEPLOY_DIR/"
cp -r R "$DEPLOY_DIR/"
cp -r data "$DEPLOY_DIR/"
cp -r www "$DEPLOY_DIR/"

# Remove unnecessary files from data directory
rm -rf "$DEPLOY_DIR/data/external" 2>/dev/null || true

cd "$DEPLOY_DIR"

# Initialize git repo and push to HF
git init
git lfs install
git lfs track "*.csv"
git add .
git commit -m "Deploy Iowa Cities Dashboard"

# Add HF remote and push
git remote add space "https://huggingface.co/spaces/$SPACE_REPO"
git push --force space main

echo ""
echo "✅ Deployment complete!"
echo ""
echo "📍 Your app will be available at:"
echo "   https://huggingface.co/spaces/$SPACE_REPO"
echo "   https://$HF_USERNAME-$SPACE_NAME.hf.space"
echo ""
echo "🌐 To set up custom domain (iowa.andernet.dev):"
echo "   1. Go to: https://huggingface.co/spaces/$SPACE_REPO/settings"
echo "   2. Scroll to 'Custom domains'"
echo "   3. Add: iowa.andernet.dev"
echo "   4. In Cloudflare, add CNAME record:"
echo "      - Name: iowa"
echo "      - Target: $HF_USERNAME-$SPACE_NAME.hf.space"
echo "      - Proxy status: DNS only (grey cloud)"
echo ""

# Cleanup
rm -rf "$DEPLOY_DIR"
