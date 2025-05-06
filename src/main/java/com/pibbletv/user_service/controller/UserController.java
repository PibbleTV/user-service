package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import com.pibbletv.user_service.domain.User;
import lombok.AllArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping("/addUser")
    public Mono<Void> addUser(@RequestParam String username) {
        return userService.addUser(username);
    }

    @PutMapping("/updateUser")
    public Mono<Void> updateUser(@RequestBody User user) {
        return userService.updateUser(user);
    }

    @PreAuthorize("hasRole('admin')")
    @PutMapping(value = "/banUser")
    public Mono<Void> banUser(@RequestParam String username) {
        return userService.banUser(username);
    }

    @PreAuthorize("hasRole('admin')")
    @PutMapping(value = "/unbanUser")
    public Mono<Void> unbanUser(@RequestParam String username) {
        return userService.unbanUser(username);
    }

    @GetMapping(value = "/getUser")
    public Mono<User> getUser(@RequestParam String username) {
        return userService.getUser(username);
    }
}