package com.pibbletv.user_service.controller;

import com.pibbletv.user_service.business.interfaces.UserService;
import lombok.AllArgsConstructor;
import com.pibbletv.user_service.domain.User;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/user")
@AllArgsConstructor
public class UserController {

    private final UserService userService;

    @PostMapping(value = "/saveUser")
    public ResponseEntity<Void> saveUser(@RequestBody Map<String, Object> data) {

        String userId = (String) data.get("userId");
        String username = (String) data.get("username");

        userService.saveUser(userId, username);

        return ResponseEntity.ok().build();
    }

    @GetMapping(value = "/getUser")
    public ResponseEntity<User> getUser(@RequestParam String userId) {
        User user = userService.getUser(userId);
        return ResponseEntity.ok(user);
    }
}