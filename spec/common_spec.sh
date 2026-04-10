Describe 'setup/common.sh'
  Include setup/common.sh

  Describe 'get_shell_rc()'
    It 'returns .zshrc on all platforms now'
      When call get_shell_rc
      The output should equal "$HOME/.zshrc"
    End
  End
  
  Describe 'sed_i()'
    It 'uses BSD sed syntax on Darwin'
      uname() { echo "Darwin"; }
      sed() { 
        # When sed_i calls `sed -i '' "$@"`, 
        # $1 is -i, $2 is '', $3 is s/foo/bar/, $4 is file.txt
        echo "called sed $1 '' $3 $4" 
      }
      
      When call sed_i "s/foo/bar/" "file.txt"
      The output should equal "called sed -i '' s/foo/bar/ file.txt"
    End

    It 'uses GNU sed syntax on Linux'
      uname() { echo "Linux"; }
      sed() { 
        # When sed_i calls `sed -i "$@"`,
        # $1 is -i, $2 is s/foo/bar/, $3 is file.txt
        echo "called sed $1 $2 $3" 
      }
      
      When call sed_i "s/foo/bar/" "file.txt"
      The output should equal "called sed -i s/foo/bar/ file.txt"
    End
  End
End
