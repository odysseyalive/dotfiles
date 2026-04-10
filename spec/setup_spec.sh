Describe 'setup.sh router'
  # Setup.sh is zsh-based
  It 'shows help when no arguments are provided'
    When run script setup.sh
    The status should be success
    The output should include 'Usage: ./setup.sh [action]'
    The output should include 'Available actions:'
  End

  It 'shows help when help argument is provided'
    When run script setup.sh help
    The status should be success
    The output should include 'Available actions:'
  End

  It 'fails gracefully with an unknown action'
    When run script setup.sh invalid_action_99
    The status should be failure
    The output should include 'Unknown action: invalid_action_99'
  End

  Describe 'subcommand routing'
    setup_fake_env() {
      mkdir -p temp_test_env/setup
      cat << 'INNER_EOF' > temp_test_env/setup.sh
#!/usr/bin/env zsh
action="${1:-help}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%x}}")" && pwd)"
if [ -x "$SCRIPT_DIR/setup/${action}.sh" ]; then
  "$SCRIPT_DIR/setup/${action}.sh"
fi
INNER_EOF
      chmod +x temp_test_env/setup.sh
      echo 'echo "tools script executed"' > temp_test_env/setup/tools.sh
      chmod +x temp_test_env/setup/tools.sh
    }
    
    BeforeEach 'setup_fake_env'
    AfterEach 'rm -rf temp_test_env'

    It 'correctly routes to a valid subcommand'
      When run script temp_test_env/setup.sh tools
      The status should be success
      The output should equal 'tools script executed'
    End
  End
End
