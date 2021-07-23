#compdef hcloud

__hcloud_bash_source() {
	alias shopt=':'
	alias _expand=_bash_expand
	alias _complete=_bash_comp
	emulate -L sh
	setopt kshglob noshglob braceexpand
	source "$@"
}
__hcloud_type() {
	# -t is not supported by zsh
	if [ "$1" == "-t" ]; then
		shift
		# fake Bash 4 to disable "complete -o nospace". Instead
		# "compopt +-o nospace" is used in the code to toggle trailing
		# spaces. We don't support that, but leave trailing spaces on
		# all the time
		if [ "$1" = "__hcloud_compopt" ]; then
			echo builtin
			return 0
		fi
	fi
	type "$@"
}
__hcloud_compgen() {
	local completions w
	completions=( $(compgen "$@") ) || return $?
	# filter by given word as prefix
	while [[ "$1" = -* && "$1" != -- ]]; do
		shift
		shift
	done
	if [[ "$1" == -- ]]; then
		shift
	fi
	for w in "${completions[@]}"; do
		if [[ "${w}" = "$1"* ]]; then
			echo "${w}"
		fi
	done
}
__hcloud_compopt() {
	true # don't do anything. Not supported by bashcompinit in zsh
}
__hcloud_declare() {
	if [ "$1" == "-F" ]; then
		whence -w "$@"
	else
		builtin declare "$@"
	fi
}
__hcloud_ltrim_colon_completions()
{
	if [[ "$1" == *:* && "$COMP_WORDBREAKS" == *:* ]]; then
		# Remove colon-word prefix from COMPREPLY items
		local colon_word=${1%${1##*:}}
		local i=${#COMPREPLY[*]}
		while [[ $((--i)) -ge 0 ]]; do
			COMPREPLY[$i]=${COMPREPLY[$i]#"$colon_word"}
		done
	fi
}
__hcloud_get_comp_words_by_ref() {
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[${COMP_CWORD}-1]}"
	words=("${COMP_WORDS[@]}")
	cword=("${COMP_CWORD[@]}")
}
__hcloud_filedir() {
	local RET OLD_IFS w qw
	__debug "_filedir $@ cur=$cur"
	if [[ "$1" = \~* ]]; then
		# somehow does not work. Maybe, zsh does not call this at all
		eval echo "$1"
		return 0
	fi
	OLD_IFS="$IFS"
	IFS=$'\n'
	if [ "$1" = "-d" ]; then
		shift
		RET=( $(compgen -d) )
	else
		RET=( $(compgen -f) )
	fi
	IFS="$OLD_IFS"
	IFS="," __debug "RET=${RET[@]} len=${#RET[@]}"
	for w in ${RET[@]}; do
		if [[ ! "${w}" = "${cur}"* ]]; then
			continue
		fi
		if eval "[[ \"\${w}\" = *.$1 || -d \"\${w}\" ]]"; then
			qw="$(__hcloud_quote "${w}")"
			if [ -d "${w}" ]; then
				COMPREPLY+=("${qw}/")
			else
				COMPREPLY+=("${qw}")
			fi
		fi
	done
}
__hcloud_quote() {
	if [[ $1 == \'* || $1 == \"* ]]; then
		# Leave out first character
		printf %q "${1:1}"
	else
		printf %q "$1"
	fi
}
autoload -U +X bashcompinit && bashcompinit
# use word boundary patterns for BSD or GNU sed
LWORD='[[:<:]]'
RWORD='[[:>:]]'
if sed --help 2>&1 | grep -q GNU; then
	LWORD='\<'
	RWORD='\>'
fi
__hcloud_convert_bash_to_zsh() {
	sed \
	-e 's/declare -F/whence -w/' \
	-e 's/_get_comp_words_by_ref "\$@"/_get_comp_words_by_ref "\$*"/' \
	-e 's/local \([a-zA-Z0-9_]*\)=/local \1; \1=/' \
	-e 's/flags+=("\(--.*\)=")/flags+=("\1"); two_word_flags+=("\1")/' \
	-e 's/must_have_one_flag+=("\(--.*\)=")/must_have_one_flag+=("\1")/' \
	-e "s/${LWORD}_filedir${RWORD}/__hcloud_filedir/g" \
	-e "s/${LWORD}_get_comp_words_by_ref${RWORD}/__hcloud_get_comp_words_by_ref/g" \
	-e "s/${LWORD}__ltrim_colon_completions${RWORD}/__hcloud_ltrim_colon_completions/g" \
	-e "s/${LWORD}compgen${RWORD}/__hcloud_compgen/g" \
	-e "s/${LWORD}compopt${RWORD}/__hcloud_compopt/g" \
	-e "s/${LWORD}declare${RWORD}/__hcloud_declare/g" \
	-e "s/\\\$(type${RWORD}/\$(__hcloud_type/g" \
	<<'BASH_COMPLETION_EOF'
# bash completion for hcloud                               -*- shell-script -*-

__hcloud_debug()
{
    if [[ -n ${BASH_COMP_DEBUG_FILE} ]]; then
        echo "$*" >> "${BASH_COMP_DEBUG_FILE}"
    fi
}

# Homebrew on Macs have version 1.3 of bash-completion which doesn't include
# _init_completion. This is a very minimal version of that function.
__hcloud_init_completion()
{
    COMPREPLY=()
    _get_comp_words_by_ref "$@" cur prev words cword
}

__hcloud_index_of_word()
{
    local w word=$1
    shift
    index=0
    for w in "$@"; do
        [[ $w = "$word" ]] && return
        index=$((index+1))
    done
    index=-1
}

__hcloud_contains_word()
{
    local w word=$1; shift
    for w in "$@"; do
        [[ $w = "$word" ]] && return
    done
    return 1
}

__hcloud_handle_go_custom_completion()
{
    __hcloud_debug "${FUNCNAME[0]}: cur is ${cur}, words[*] is ${words[*]}, #words[@] is ${#words[@]}"

    local shellCompDirectiveError=1
    local shellCompDirectiveNoSpace=2
    local shellCompDirectiveNoFileComp=4
    local shellCompDirectiveFilterFileExt=8
    local shellCompDirectiveFilterDirs=16

    local out requestComp lastParam lastChar comp directive args

    # Prepare the command to request completions for the program.
    # Calling ${words[0]} instead of directly hcloud allows to handle aliases
    args=("${words[@]:1}")
    requestComp="${words[0]} __completeNoDesc ${args[*]}"

    lastParam=${words[$((${#words[@]}-1))]}
    lastChar=${lastParam:$((${#lastParam}-1)):1}
    __hcloud_debug "${FUNCNAME[0]}: lastParam ${lastParam}, lastChar ${lastChar}"

    if [ -z "${cur}" ] && [ "${lastChar}" != "=" ]; then
        # If the last parameter is complete (there is a space following it)
        # We add an extra empty parameter so we can indicate this to the go method.
        __hcloud_debug "${FUNCNAME[0]}: Adding extra empty parameter"
        requestComp="${requestComp} \"\""
    fi

    __hcloud_debug "${FUNCNAME[0]}: calling ${requestComp}"
    # Use eval to handle any environment variables and such
    out=$(eval "${requestComp}" 2>/dev/null)

    # Extract the directive integer at the very end of the output following a colon (:)
    directive=${out##*:}
    # Remove the directive
    out=${out%:*}
    if [ "${directive}" = "${out}" ]; then
        # There is not directive specified
        directive=0
    fi
    __hcloud_debug "${FUNCNAME[0]}: the completion directive is: ${directive}"
    __hcloud_debug "${FUNCNAME[0]}: the completions are: ${out[*]}"

    if [ $((directive & shellCompDirectiveError)) -ne 0 ]; then
        # Error code.  No completion.
        __hcloud_debug "${FUNCNAME[0]}: received error from custom completion go code"
        return
    else
        if [ $((directive & shellCompDirectiveNoSpace)) -ne 0 ]; then
            if [[ $(type -t compopt) = "builtin" ]]; then
                __hcloud_debug "${FUNCNAME[0]}: activating no space"
                compopt -o nospace
            fi
        fi
        if [ $((directive & shellCompDirectiveNoFileComp)) -ne 0 ]; then
            if [[ $(type -t compopt) = "builtin" ]]; then
                __hcloud_debug "${FUNCNAME[0]}: activating no file completion"
                compopt +o default
            fi
        fi
    fi

    if [ $((directive & shellCompDirectiveFilterFileExt)) -ne 0 ]; then
        # File extension filtering
        local fullFilter filter filteringCmd
        # Do not use quotes around the $out variable or else newline
        # characters will be kept.
        for filter in ${out[*]}; do
            fullFilter+="$filter|"
        done

        filteringCmd="_filedir $fullFilter"
        __hcloud_debug "File filtering command: $filteringCmd"
        $filteringCmd
    elif [ $((directive & shellCompDirectiveFilterDirs)) -ne 0 ]; then
        # File completion for directories only
        local subDir
        # Use printf to strip any trailing newline
        subdir=$(printf "%s" "${out[0]}")
        if [ -n "$subdir" ]; then
            __hcloud_debug "Listing directories in $subdir"
            __hcloud_handle_subdirs_in_dir_flag "$subdir"
        else
            __hcloud_debug "Listing directories in ."
            _filedir -d
        fi
    else
        while IFS='' read -r comp; do
            COMPREPLY+=("$comp")
        done < <(compgen -W "${out[*]}" -- "$cur")
    fi
}

__hcloud_handle_reply()
{
    __hcloud_debug "${FUNCNAME[0]}"
    local comp
    case $cur in
        -*)
            if [[ $(type -t compopt) = "builtin" ]]; then
                compopt -o nospace
            fi
            local allflags
            if [ ${#must_have_one_flag[@]} -ne 0 ]; then
                allflags=("${must_have_one_flag[@]}")
            else
                allflags=("${flags[*]} ${two_word_flags[*]}")
            fi
            while IFS='' read -r comp; do
                COMPREPLY+=("$comp")
            done < <(compgen -W "${allflags[*]}" -- "$cur")
            if [[ $(type -t compopt) = "builtin" ]]; then
                [[ "${COMPREPLY[0]}" == *= ]] || compopt +o nospace
            fi

            # complete after --flag=abc
            if [[ $cur == *=* ]]; then
                if [[ $(type -t compopt) = "builtin" ]]; then
                    compopt +o nospace
                fi

                local index flag
                flag="${cur%=*}"
                __hcloud_index_of_word "${flag}" "${flags_with_completion[@]}"
                COMPREPLY=()
                if [[ ${index} -ge 0 ]]; then
                    PREFIX=""
                    cur="${cur#*=}"
                    ${flags_completion[${index}]}
                    if [ -n "${ZSH_VERSION}" ]; then
                        # zsh completion needs --flag= prefix
                        eval "COMPREPLY=( \"\${COMPREPLY[@]/#/${flag}=}\" )"
                    fi
                fi
            fi
            return 0;
            ;;
    esac

    # check if we are handling a flag with special work handling
    local index
    __hcloud_index_of_word "${prev}" "${flags_with_completion[@]}"
    if [[ ${index} -ge 0 ]]; then
        ${flags_completion[${index}]}
        return
    fi

    # we are parsing a flag and don't have a special handler, no completion
    if [[ ${cur} != "${words[cword]}" ]]; then
        return
    fi

    local completions
    completions=("${commands[@]}")
    if [[ ${#must_have_one_noun[@]} -ne 0 ]]; then
        completions+=("${must_have_one_noun[@]}")
    elif [[ -n "${has_completion_function}" ]]; then
        # if a go completion function is provided, defer to that function
        __hcloud_handle_go_custom_completion
    fi
    if [[ ${#must_have_one_flag[@]} -ne 0 ]]; then
        completions+=("${must_have_one_flag[@]}")
    fi
    while IFS='' read -r comp; do
        COMPREPLY+=("$comp")
    done < <(compgen -W "${completions[*]}" -- "$cur")

    if [[ ${#COMPREPLY[@]} -eq 0 && ${#noun_aliases[@]} -gt 0 && ${#must_have_one_noun[@]} -ne 0 ]]; then
        while IFS='' read -r comp; do
            COMPREPLY+=("$comp")
        done < <(compgen -W "${noun_aliases[*]}" -- "$cur")
    fi

    if [[ ${#COMPREPLY[@]} -eq 0 ]]; then
		if declare -F __hcloud_custom_func >/dev/null; then
			# try command name qualified custom func
			__hcloud_custom_func
		else
			# otherwise fall back to unqualified for compatibility
			declare -F __custom_func >/dev/null && __custom_func
		fi
    fi

    # available in bash-completion >= 2, not always present on macOS
    if declare -F __ltrim_colon_completions >/dev/null; then
        __ltrim_colon_completions "$cur"
    fi

    # If there is only 1 completion and it is a flag with an = it will be completed
    # but we don't want a space after the =
    if [[ "${#COMPREPLY[@]}" -eq "1" ]] && [[ $(type -t compopt) = "builtin" ]] && [[ "${COMPREPLY[0]}" == --*= ]]; then
       compopt -o nospace
    fi
}

# The arguments should be in the form "ext1|ext2|extn"
__hcloud_handle_filename_extension_flag()
{
    local ext="$1"
    _filedir "@(${ext})"
}

__hcloud_handle_subdirs_in_dir_flag()
{
    local dir="$1"
    pushd "${dir}" >/dev/null 2>&1 && _filedir -d && popd >/dev/null 2>&1 || return
}

__hcloud_handle_flag()
{
    __hcloud_debug "${FUNCNAME[0]}: c is $c words[c] is ${words[c]}"

    # if a command required a flag, and we found it, unset must_have_one_flag()
    local flagname=${words[c]}
    local flagvalue
    # if the word contained an =
    if [[ ${words[c]} == *"="* ]]; then
        flagvalue=${flagname#*=} # take in as flagvalue after the =
        flagname=${flagname%=*} # strip everything after the =
        flagname="${flagname}=" # but put the = back
    fi
    __hcloud_debug "${FUNCNAME[0]}: looking for ${flagname}"
    if __hcloud_contains_word "${flagname}" "${must_have_one_flag[@]}"; then
        must_have_one_flag=()
    fi

    # if you set a flag which only applies to this command, don't show subcommands
    if __hcloud_contains_word "${flagname}" "${local_nonpersistent_flags[@]}"; then
      commands=()
    fi

    # keep flag value with flagname as flaghash
    # flaghash variable is an associative array which is only supported in bash > 3.
    if [[ -z "${BASH_VERSION}" || "${BASH_VERSINFO[0]}" -gt 3 ]]; then
        if [ -n "${flagvalue}" ] ; then
            flaghash[${flagname}]=${flagvalue}
        elif [ -n "${words[ $((c+1)) ]}" ] ; then
            flaghash[${flagname}]=${words[ $((c+1)) ]}
        else
            flaghash[${flagname}]="true" # pad "true" for bool flag
        fi
    fi

    # skip the argument to a two word flag
    if [[ ${words[c]} != *"="* ]] && __hcloud_contains_word "${words[c]}" "${two_word_flags[@]}"; then
			  __hcloud_debug "${FUNCNAME[0]}: found a flag ${words[c]}, skip the next argument"
        c=$((c+1))
        # if we are looking for a flags value, don't show commands
        if [[ $c -eq $cword ]]; then
            commands=()
        fi
    fi

    c=$((c+1))

}

__hcloud_handle_noun()
{
    __hcloud_debug "${FUNCNAME[0]}: c is $c words[c] is ${words[c]}"

    if __hcloud_contains_word "${words[c]}" "${must_have_one_noun[@]}"; then
        must_have_one_noun=()
    elif __hcloud_contains_word "${words[c]}" "${noun_aliases[@]}"; then
        must_have_one_noun=()
    fi

    nouns+=("${words[c]}")
    c=$((c+1))
}

__hcloud_handle_command()
{
    __hcloud_debug "${FUNCNAME[0]}: c is $c words[c] is ${words[c]}"

    local next_command
    if [[ -n ${last_command} ]]; then
        next_command="_${last_command}_${words[c]//:/__}"
    else
        if [[ $c -eq 0 ]]; then
            next_command="_hcloud_root_command"
        else
            next_command="_${words[c]//:/__}"
        fi
    fi
    c=$((c+1))
    __hcloud_debug "${FUNCNAME[0]}: looking for ${next_command}"
    declare -F "$next_command" >/dev/null && $next_command
}

__hcloud_handle_word()
{
    if [[ $c -ge $cword ]]; then
        __hcloud_handle_reply
        return
    fi
    __hcloud_debug "${FUNCNAME[0]}: c is $c words[c] is ${words[c]}"
    if [[ "${words[c]}" == -* ]]; then
        __hcloud_handle_flag
    elif __hcloud_contains_word "${words[c]}" "${commands[@]}"; then
        __hcloud_handle_command
    elif [[ $c -eq 0 ]]; then
        __hcloud_handle_command
    elif __hcloud_contains_word "${words[c]}" "${command_aliases[@]}"; then
        # aliashash variable is an associative array which is only supported in bash > 3.
        if [[ -z "${BASH_VERSION}" || "${BASH_VERSINFO[0]}" -gt 3 ]]; then
            words[c]=${aliashash[${words[c]}]}
            __hcloud_handle_command
        else
            __hcloud_handle_noun
        fi
    else
        __hcloud_handle_noun
    fi
    __hcloud_handle_word
}


	__hcloud_sshkey_names() {
		local ctl_output out
		if ctl_output=$(hcloud ssh-key list -o noheader -o columns=name 2>/dev/null); then
			IFS=$'\n'
			COMPREPLY=($(echo "${ctl_output}" | while read -r line; do printf "%q\n" "$line"; done))
		fi
	}

	__hcloud_context_names() {
		local ctl_output out
		if ctl_output=$(hcloud context list -o noheader 2>/dev/null); then
			IFS=$'\n'
			COMPREPLY=($(echo "${ctl_output}" | while read -r line; do printf "%q\n" "$line"; done))
		fi
	}

	__hcloud_floatingip_ids() {
		local ctl_output out
		if ctl_output=$(hcloud floating-ip list -o noheader -o columns=id 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_volume_names() {
		local ctl_output out
		if ctl_output=$(hcloud volume list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_network_names() {
		local ctl_output out
		if ctl_output=$(hcloud network list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_iso_names() {
		local ctl_output out
		if ctl_output=$(hcloud iso list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_datacenter_names() {
		local ctl_output out
		if ctl_output=$(hcloud datacenter list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_location_names() {
		local ctl_output out
		if ctl_output=$(hcloud location list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_server_names() {
		local ctl_output out
		if ctl_output=$(hcloud server list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_servertype_names() {
		local ctl_output out
		if ctl_output=$(hcloud server-type list -o noheader -o columns=name 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}"))
		fi
	}

	__hcloud_image_ids_no_system() {
		local ctl_output out
		if ctl_output=$(hcloud image list -o noheader 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}" | awk '{if ($2 != "system") {print $1}}'))
		fi
	}

	__hcloud_image_names() {
		local ctl_output out
		if ctl_output=$(hcloud image list -o noheader 2>/dev/null); then
				COMPREPLY=($(echo "${ctl_output}" | awk '{if ($3 == "-") {print $1} else {print $3}}'))
		fi
	}

	__hcloud_floating_ip_ids() {
		local ctl_output out
		if ctl_output=$(hcloud floating-ip list -o noheader 2>/dev/null); then
			COMPREPLY=($(echo "${ctl_output}" | awk '{print $1}'))
		fi
	}

	__hcloud_image_types_no_system() {
		COMPREPLY=($(echo "snapshot backup"))
	}

	__hcloud_protection_levels() {
		COMPREPLY=($(echo "delete"))
	}

	__hcloud_server_protection_levels() {
		COMPREPLY=($(echo "delete rebuild"))
	}

	__hcloud_floatingip_types() {
		COMPREPLY=($(echo "ipv4 ipv6"))
	}

	__hcloud_rescue_types() {
		COMPREPLY=($(echo "linux64 linux32 freebsd64"))
	}

	__hcloud_network_zones() {
		COMPREPLY=($(echo "eu-central"))
	}

	__hcloud_network_subnet_types() {
		COMPREPLY=($(echo "server"))
	}

	__custom_func() {
		case ${last_command} in
			hcloud_server_delete | hcloud_server_describe | \
			hcloud_server_create-image | hcloud_server_poweron | \
			hcloud_server_poweroff | hcloud_server_reboot | \
			hcloud_server_reset | hcloud_server_reset-password | \
			hcloud_server_shutdown | hcloud_server_disable-rescue | \
			hcloud_server_enable-rescue | hcloud_server_detach-iso | \
			hcloud_server_update | hcloud_server_enable-backup | \
			hcloud_server_disable-backup | hcloud_server_rebuild | \
			hcloud_server_add-label | hcloud_server_remove-label )
				__hcloud_server_names
				return
				;;
			hcloud_server_attach-iso )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_iso_names
					return
				fi
				__hcloud_server_names
				return
				;;
			hcloud_server_change-type )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_servertype_names
					return
				fi
				__hcloud_server_names
				return
				;;
			hcloud_server-type_describe )
				__hcloud_servertype_names
				return
				;;
			hcloud_image_describe | hcloud_image_add-label | hcloud_image_remove-label )
				__hcloud_image_names
				return
				;;
			hcloud_image_delete | hcloud_image_update )
				__hcloud_image_ids_no_system
				return
				;;
			hcloud_floating-ip_assign )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_server_names
					return
				fi
				__hcloud_floating_ip_ids
				return
				;;
			hcloud_floating-ip_enable-protection | hcloud_floating-ip_disable-protection )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_protection_levels
					return
				fi
				__hcloud_floating_ip_ids
				return
				;;
			hcloud_image_enable-protection | hcloud_image_disable-protection )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_protection_levels
					return
				fi
				__hcloud_image_ids_no_system
				return
				;;
			hcloud_server_enable-protection | hcloud_server_disable-protection )
				if [[ ${#nouns[@]} -gt 2 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -gt 0 ]]; then
					__hcloud_server_protection_levels
					return
				fi
				__hcloud_server_names
				return
				;;
			hcloud_volumes_enable-protection | hcloud_volume_disable-protection )
				if [[ ${#nouns[@]} -gt 1 ]]; then
					return 1
				fi
				if [[ ${#nouns[@]} -eq 1 ]]; then
					__hcloud_protection_levels
					return
				fi
				__hcloud_volume_names
				return
				;;
			hcloud_floating-ip_unassign | hcloud_floating-ip_delete | \
			hcloud_floating-ip_describe | hcloud_floating-ip_update | \
			hcloud_floating-ip_add-label | hcloud_floating-ip_remove-label )
				__hcloud_floating_ip_ids
				return
				;;
            hcloud_volume_detach | hcloud_volume_delete | \
			hcloud_volume_describe | hcloud_volume_update | \
			hcloud_volume_add-label | hcloud_volume_remove-label )
				__hcloud_volume_names
				return
				;;
			hcloud_datacenter_describe )
				__hcloud_datacenter_names
				return
				;;
			hcloud_location_describe )
				__hcloud_location_names
				return
				;;
			hcloud_iso_describe )
				__hcloud_iso_names
				return
				;;
			hcloud_context_use | hcloud_context_delete )
				__hcloud_context_names
				return
				;;
			hcloud_ssh-key_delete | hcloud_ssh-key_describe | \
			hcloud_ssh-key_add-label | hcloud_ssk-key_remove-label)
				__hcloud_sshkey_names
				return
				;;
			*)
				;;
		esac
	}
	
_hcloud_completion()
{
    last_command="hcloud_completion"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--help")
    flags+=("-h")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    must_have_one_noun+=("bash")
    must_have_one_noun+=("zsh")
    noun_aliases=()
}

_hcloud_context_active()
{
    last_command="hcloud_context_active"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_context_create()
{
    last_command="hcloud_context_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_context_delete()
{
    last_command="hcloud_context_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_context_list()
{
    last_command="hcloud_context_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_context_use()
{
    last_command="hcloud_context_use"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_context()
{
    last_command="hcloud_context"

    command_aliases=()

    commands=()
    commands+=("active")
    commands+=("create")
    commands+=("delete")
    commands+=("list")
    commands+=("use")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_datacenter_describe()
{
    last_command="hcloud_datacenter_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_datacenter_list()
{
    last_command="hcloud_datacenter_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_datacenter()
{
    last_command="hcloud_datacenter"

    command_aliases=()

    commands=()
    commands+=("describe")
    commands+=("list")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_add-label()
{
    last_command="hcloud_floating-ip_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_assign()
{
    last_command="hcloud_floating-ip_assign"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_create()
{
    last_command="hcloud_floating-ip_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--description=")
    two_word_flags+=("--description")
    flags+=("--home-location=")
    two_word_flags+=("--home-location")
    flags_with_completion+=("--home-location")
    flags_completion+=("__hcloud_location_names")
    flags+=("--server=")
    two_word_flags+=("--server")
    flags_with_completion+=("--server")
    flags_completion+=("__hcloud_server_names")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_floatingip_types")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--type=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_delete()
{
    last_command="hcloud_floating-ip_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_describe()
{
    last_command="hcloud_floating-ip_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_disable-protection()
{
    last_command="hcloud_floating-ip_disable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_enable-protection()
{
    last_command="hcloud_floating-ip_enable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_list()
{
    last_command="hcloud_floating-ip_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_remove-label()
{
    last_command="hcloud_floating-ip_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_set-rdns()
{
    last_command="hcloud_floating-ip_set-rdns"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--hostname=")
    two_word_flags+=("--hostname")
    two_word_flags+=("-r")
    flags+=("--ip=")
    two_word_flags+=("--ip")
    two_word_flags+=("-i")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--hostname=")
    must_have_one_flag+=("-r")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_unassign()
{
    last_command="hcloud_floating-ip_unassign"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip_update()
{
    last_command="hcloud_floating-ip_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--description=")
    two_word_flags+=("--description")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_floating-ip()
{
    last_command="hcloud_floating-ip"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("assign")
    commands+=("create")
    commands+=("delete")
    commands+=("describe")
    commands+=("disable-protection")
    commands+=("enable-protection")
    commands+=("list")
    commands+=("remove-label")
    commands+=("set-rdns")
    commands+=("unassign")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_help()
{
    last_command="hcloud_help"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    has_completion_function=1
    noun_aliases=()
}

_hcloud_image_add-label()
{
    last_command="hcloud_image_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_delete()
{
    last_command="hcloud_image_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_describe()
{
    last_command="hcloud_image_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_disable-protection()
{
    last_command="hcloud_image_disable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_enable-protection()
{
    last_command="hcloud_image_enable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_list()
{
    last_command="hcloud_image_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--type=")
    two_word_flags+=("--type")
    two_word_flags+=("-t")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_remove-label()
{
    last_command="hcloud_image_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image_update()
{
    last_command="hcloud_image_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--description=")
    two_word_flags+=("--description")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_image_types_no_system")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_image()
{
    last_command="hcloud_image"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("delete")
    commands+=("describe")
    commands+=("disable-protection")
    commands+=("enable-protection")
    commands+=("list")
    commands+=("remove-label")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_iso_describe()
{
    last_command="hcloud_iso_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_iso_list()
{
    last_command="hcloud_iso_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_iso()
{
    last_command="hcloud_iso"

    command_aliases=()

    commands=()
    commands+=("describe")
    commands+=("list")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_location_describe()
{
    last_command="hcloud_location_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_location_list()
{
    last_command="hcloud_location_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_location()
{
    last_command="hcloud_location"

    command_aliases=()

    commands=()
    commands+=("describe")
    commands+=("list")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_add-label()
{
    last_command="hcloud_network_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_add-route()
{
    last_command="hcloud_network_add-route"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--destination=")
    two_word_flags+=("--destination")
    flags+=("--gateway=")
    two_word_flags+=("--gateway")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--destination=")
    must_have_one_flag+=("--gateway=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_add-subnet()
{
    last_command="hcloud_network_add-subnet"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ip-range=")
    two_word_flags+=("--ip-range")
    flags+=("--network-zone=")
    two_word_flags+=("--network-zone")
    flags_with_completion+=("--network-zone")
    flags_completion+=("__hcloud_network_zones")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_network_subnet_types")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--ip-range=")
    must_have_one_flag+=("--network-zone=")
    must_have_one_flag+=("--type=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_change-ip-range()
{
    last_command="hcloud_network_change-ip-range"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ip-range=")
    two_word_flags+=("--ip-range")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--ip-range=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_create()
{
    last_command="hcloud_network_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ip-range=")
    two_word_flags+=("--ip-range")
    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--ip-range=")
    must_have_one_flag+=("--name=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_delete()
{
    last_command="hcloud_network_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_describe()
{
    last_command="hcloud_network_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_disable-protection()
{
    last_command="hcloud_network_disable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_enable-protection()
{
    last_command="hcloud_network_enable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_list()
{
    last_command="hcloud_network_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_remove-label()
{
    last_command="hcloud_network_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_remove-route()
{
    last_command="hcloud_network_remove-route"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--destination=")
    two_word_flags+=("--destination")
    flags+=("--gateway=")
    two_word_flags+=("--gateway")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--destination=")
    must_have_one_flag+=("--gateway=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_remove-subnet()
{
    last_command="hcloud_network_remove-subnet"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ip-range=")
    two_word_flags+=("--ip-range")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--ip-range=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network_update()
{
    last_command="hcloud_network_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_network()
{
    last_command="hcloud_network"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("add-route")
    commands+=("add-subnet")
    commands+=("change-ip-range")
    commands+=("create")
    commands+=("delete")
    commands+=("describe")
    commands+=("disable-protection")
    commands+=("enable-protection")
    commands+=("list")
    commands+=("remove-label")
    commands+=("remove-route")
    commands+=("remove-subnet")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_add-label()
{
    last_command="hcloud_server_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_attach-iso()
{
    last_command="hcloud_server_attach-iso"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_attach-to-network()
{
    last_command="hcloud_server_attach-to-network"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--alias-ips=")
    two_word_flags+=("--alias-ips")
    flags+=("--ip=")
    two_word_flags+=("--ip")
    flags+=("--network=")
    two_word_flags+=("--network")
    flags_with_completion+=("--network")
    flags_completion+=("__hcloud_network_names")
    two_word_flags+=("-n")
    flags_with_completion+=("-n")
    flags_completion+=("__hcloud_network_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--network=")
    must_have_one_flag+=("-n")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_change-alias-ips()
{
    last_command="hcloud_server_change-alias-ips"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--alias-ips=")
    two_word_flags+=("--alias-ips")
    flags+=("--clear")
    flags+=("--network=")
    two_word_flags+=("--network")
    flags_with_completion+=("--network")
    flags_completion+=("__hcloud_network_names")
    two_word_flags+=("-n")
    flags_with_completion+=("-n")
    flags_completion+=("__hcloud_network_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--network=")
    must_have_one_flag+=("-n")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_change-type()
{
    last_command="hcloud_server_change-type"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--keep-disk")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_create()
{
    last_command="hcloud_server_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--automount")
    flags+=("--datacenter=")
    two_word_flags+=("--datacenter")
    flags_with_completion+=("--datacenter")
    flags_completion+=("__hcloud_datacenter_names")
    flags+=("--image=")
    two_word_flags+=("--image")
    flags_with_completion+=("--image")
    flags_completion+=("__hcloud_image_names")
    flags+=("--location=")
    two_word_flags+=("--location")
    flags_with_completion+=("--location")
    flags_completion+=("__hcloud_location_names")
    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--network=")
    two_word_flags+=("--network")
    flags+=("--ssh-key=")
    two_word_flags+=("--ssh-key")
    flags_with_completion+=("--ssh-key")
    flags_completion+=("__hcloud_sshkey_names")
    flags+=("--start-after-create")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_servertype_names")
    flags+=("--user-data-from-file=")
    two_word_flags+=("--user-data-from-file")
    flags+=("--volume=")
    two_word_flags+=("--volume")
    flags_with_completion+=("--volume")
    flags_completion+=("__hcloud_volume_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--image=")
    must_have_one_flag+=("--name=")
    must_have_one_flag+=("--type=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_create-image()
{
    last_command="hcloud_server_create-image"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--description=")
    two_word_flags+=("--description")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_image_types_no_system")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--type=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_delete()
{
    last_command="hcloud_server_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_describe()
{
    last_command="hcloud_server_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_detach-from-network()
{
    last_command="hcloud_server_detach-from-network"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--network=")
    two_word_flags+=("--network")
    flags_with_completion+=("--network")
    flags_completion+=("__hcloud_network_names")
    two_word_flags+=("-n")
    flags_with_completion+=("-n")
    flags_completion+=("__hcloud_network_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--network=")
    must_have_one_flag+=("-n")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_detach-iso()
{
    last_command="hcloud_server_detach-iso"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_disable-backup()
{
    last_command="hcloud_server_disable-backup"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_disable-protection()
{
    last_command="hcloud_server_disable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_disable-rescue()
{
    last_command="hcloud_server_disable-rescue"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_enable-backup()
{
    last_command="hcloud_server_enable-backup"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--window=")
    two_word_flags+=("--window")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_enable-protection()
{
    last_command="hcloud_server_enable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_enable-rescue()
{
    last_command="hcloud_server_enable-rescue"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ssh-key=")
    two_word_flags+=("--ssh-key")
    flags_with_completion+=("--ssh-key")
    flags_completion+=("__hcloud_sshkey_names")
    flags+=("--type=")
    two_word_flags+=("--type")
    flags_with_completion+=("--type")
    flags_completion+=("__hcloud_rescue_types")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_list()
{
    last_command="hcloud_server_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_poweroff()
{
    last_command="hcloud_server_poweroff"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_poweron()
{
    last_command="hcloud_server_poweron"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_reboot()
{
    last_command="hcloud_server_reboot"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_rebuild()
{
    last_command="hcloud_server_rebuild"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--image=")
    two_word_flags+=("--image")
    flags_with_completion+=("--image")
    flags_completion+=("__hcloud_image_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--image=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_remove-label()
{
    last_command="hcloud_server_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_reset()
{
    last_command="hcloud_server_reset"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_reset-password()
{
    last_command="hcloud_server_reset-password"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_set-rdns()
{
    last_command="hcloud_server_set-rdns"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--hostname=")
    two_word_flags+=("--hostname")
    two_word_flags+=("-r")
    flags+=("--ip=")
    two_word_flags+=("--ip")
    two_word_flags+=("-i")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--hostname=")
    must_have_one_flag+=("-r")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_shutdown()
{
    last_command="hcloud_server_shutdown"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_ssh()
{
    last_command="hcloud_server_ssh"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--ipv6")
    flags+=("--port=")
    two_word_flags+=("--port")
    two_word_flags+=("-p")
    flags+=("--user=")
    two_word_flags+=("--user")
    two_word_flags+=("-u")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server_update()
{
    last_command="hcloud_server_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server()
{
    last_command="hcloud_server"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("attach-iso")
    commands+=("attach-to-network")
    commands+=("change-alias-ips")
    commands+=("change-type")
    commands+=("create")
    commands+=("create-image")
    commands+=("delete")
    commands+=("describe")
    commands+=("detach-from-network")
    commands+=("detach-iso")
    commands+=("disable-backup")
    commands+=("disable-protection")
    commands+=("disable-rescue")
    commands+=("enable-backup")
    commands+=("enable-protection")
    commands+=("enable-rescue")
    commands+=("list")
    commands+=("poweroff")
    commands+=("poweron")
    commands+=("reboot")
    commands+=("rebuild")
    commands+=("remove-label")
    commands+=("reset")
    commands+=("reset-password")
    commands+=("set-rdns")
    commands+=("shutdown")
    commands+=("ssh")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server-type_describe()
{
    last_command="hcloud_server-type_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server-type_list()
{
    last_command="hcloud_server-type_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_server-type()
{
    last_command="hcloud_server-type"

    command_aliases=()

    commands=()
    commands+=("describe")
    commands+=("list")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_add-label()
{
    last_command="hcloud_ssh-key_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_create()
{
    last_command="hcloud_ssh-key_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--public-key=")
    two_word_flags+=("--public-key")
    flags+=("--public-key-from-file=")
    two_word_flags+=("--public-key-from-file")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_delete()
{
    last_command="hcloud_ssh-key_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_describe()
{
    last_command="hcloud_ssh-key_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_list()
{
    last_command="hcloud_ssh-key_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_remove-label()
{
    last_command="hcloud_ssh-key_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key_update()
{
    last_command="hcloud_ssh-key_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_ssh-key()
{
    last_command="hcloud_ssh-key"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("create")
    commands+=("delete")
    commands+=("describe")
    commands+=("list")
    commands+=("remove-label")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_version()
{
    last_command="hcloud_version"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_add-label()
{
    last_command="hcloud_volume_add-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--overwrite")
    flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_attach()
{
    last_command="hcloud_volume_attach"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--automount")
    flags+=("--server=")
    two_word_flags+=("--server")
    flags_with_completion+=("--server")
    flags_completion+=("__hcloud_server_names")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--server=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_create()
{
    last_command="hcloud_volume_create"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--automount")
    flags+=("--format=")
    two_word_flags+=("--format")
    flags+=("--location=")
    two_word_flags+=("--location")
    flags_with_completion+=("--location")
    flags_completion+=("__hcloud_location_names")
    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--server=")
    two_word_flags+=("--server")
    flags_with_completion+=("--server")
    flags_completion+=("__hcloud_server_names")
    flags+=("--size=")
    two_word_flags+=("--size")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--name=")
    must_have_one_flag+=("--size=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_delete()
{
    last_command="hcloud_volume_delete"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_describe()
{
    last_command="hcloud_volume_describe"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_detach()
{
    last_command="hcloud_volume_detach"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_disable-protection()
{
    last_command="hcloud_volume_disable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_enable-protection()
{
    last_command="hcloud_volume_enable-protection"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_list()
{
    last_command="hcloud_volume_list"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--output=")
    two_word_flags+=("--output")
    two_word_flags+=("-o")
    flags+=("--selector=")
    two_word_flags+=("--selector")
    two_word_flags+=("-l")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_remove-label()
{
    last_command="hcloud_volume_remove-label"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--all")
    flags+=("-a")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_resize()
{
    last_command="hcloud_volume_resize"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--size=")
    two_word_flags+=("--size")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_flag+=("--size=")
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume_update()
{
    last_command="hcloud_volume_update"

    command_aliases=()

    commands=()

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--name=")
    two_word_flags+=("--name")
    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_volume()
{
    last_command="hcloud_volume"

    command_aliases=()

    commands=()
    commands+=("add-label")
    commands+=("attach")
    commands+=("create")
    commands+=("delete")
    commands+=("describe")
    commands+=("detach")
    commands+=("disable-protection")
    commands+=("enable-protection")
    commands+=("list")
    commands+=("remove-label")
    commands+=("resize")
    commands+=("update")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

_hcloud_root_command()
{
    last_command="hcloud"

    command_aliases=()

    commands=()
    commands+=("completion")
    commands+=("context")
    commands+=("datacenter")
    commands+=("floating-ip")
    commands+=("help")
    commands+=("image")
    commands+=("iso")
    commands+=("location")
    commands+=("network")
    commands+=("server")
    commands+=("server-type")
    commands+=("ssh-key")
    commands+=("version")
    commands+=("volume")

    flags=()
    two_word_flags=()
    local_nonpersistent_flags=()
    flags_with_completion=()
    flags_completion=()

    flags+=("--poll-interval=")
    two_word_flags+=("--poll-interval")

    must_have_one_flag=()
    must_have_one_noun=()
    noun_aliases=()
}

__start_hcloud()
{
    local cur prev words cword
    declare -A flaghash 2>/dev/null || :
    declare -A aliashash 2>/dev/null || :
    if declare -F _init_completion >/dev/null 2>&1; then
        _init_completion -s || return
    else
        __hcloud_init_completion -n "=" || return
    fi

    local c=0
    local flags=()
    local two_word_flags=()
    local local_nonpersistent_flags=()
    local flags_with_completion=()
    local flags_completion=()
    local commands=("hcloud")
    local must_have_one_flag=()
    local must_have_one_noun=()
    local has_completion_function
    local last_command
    local nouns=()

    __hcloud_handle_word
}

if [[ $(type -t compopt) = "builtin" ]]; then
    complete -o default -F __start_hcloud hcloud
else
    complete -o default -o nospace -F __start_hcloud hcloud
fi

# ex: ts=4 sw=4 et filetype=sh

BASH_COMPLETION_EOF
}
__hcloud_bash_source <(__hcloud_convert_bash_to_zsh)
