# Iowa Cities R Project - Shell Environment
# Source this file: source .env.sh
# ==========================================

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Project root
export R_PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]:-$0}")" && pwd)"

# =============================================================================
# HELPFUL ALIASES
# =============================================================================

# Quick R commands
alias r='Rscript -e'
alias renv-activate='Rscript -e "source(\"renv/activate.R\")"'

# Run tests with nice output
rtest() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}🧪 Running Tests${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    Rscript -e "source('renv/activate.R'); testthat::test_dir('tests/testthat', stop_on_failure = FALSE)"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Run the Shiny app
rapp() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}🚀 Starting Iowa Cities Dashboard${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    Rscript -e "source('renv/activate.R'); shiny::runApp('app.R', launch.browser = TRUE)"
}

# Run the API
rapi() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}🔌 Starting Iowa Cities API on port ${1:-8000}${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    Rscript -e "source('renv/activate.R'); plumber::plumb('api.R')\$run(host='0.0.0.0', port=${1:-8000})"
}

# Check project status
rstatus() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}📊 Project Status${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    
    echo -e "\n${YELLOW}📁 Data Files:${NC}"
    ls -lh data/raw/*.csv 2>/dev/null | awk '{print "   " $9 " (" $5 ")"}'
    
    echo -e "\n${YELLOW}📦 renv Status:${NC}"
    Rscript -e "source('renv/activate.R'); status <- renv::status(); if(status\$synchronized) cat('   ✅ All packages synchronized\n') else cat('   ⚠️  Out of sync - run renv::restore()\n')" 2>/dev/null
    
    echo -e "\n${YELLOW}🔧 Git Status:${NC}"
    git status --short | head -10 | sed 's/^/   /'
    
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# Lint R files
rlint() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}🔍 Linting R Files${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    if [ -n "$1" ]; then
        Rscript -e "source('renv/activate.R'); lintr::lint('$1')"
    else
        Rscript -e "source('renv/activate.R'); lintr::lint_dir('R')"
    fi
}

# Restore packages
rrestore() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}📦 Restoring renv Packages${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    cd "$R_PROJECT_ROOT"
    Rscript -e "source('renv/activate.R'); renv::restore(prompt = FALSE)"
}

# Quick help
rhelp() {
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BOLD}📖 Iowa Cities R Project - Commands${NC}"
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo -e "  ${GREEN}rtest${NC}      Run all unit tests"
    echo -e "  ${GREEN}rapp${NC}       Start the Shiny dashboard"
    echo -e "  ${GREEN}rapi${NC}       Start the REST API (default port 8000)"
    echo -e "  ${GREEN}rapi 3000${NC}  Start the REST API on port 3000"
    echo -e "  ${GREEN}rstatus${NC}    Show project status"
    echo -e "  ${GREEN}rlint${NC}      Lint R files in R/ directory"
    echo -e "  ${GREEN}rlint file${NC} Lint a specific file"
    echo -e "  ${GREEN}rrestore${NC}   Restore renv packages"
    echo -e "  ${GREEN}r 'expr'${NC}   Run R expression (e.g., r 'print(1+1)')"
    echo ""
    echo -e "${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

# =============================================================================
# AUTO-ACTIVATE
# =============================================================================

# Show welcome message when sourced
echo -e "${GREEN}✓${NC} Iowa Cities R Project environment loaded"
echo -e "  Type ${CYAN}rhelp${NC} for available commands"
